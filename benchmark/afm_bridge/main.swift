//
// afm_bridge - Apple Foundation Models (AFM) を OpenAI Chat Completions API 互換の
//              HTTPエンドポイントとして公開する開発用ブリッジ (Issue #180)
//
// benchmark/sumibi_bench.py は OpenAI 互換API前提なので、SUMIBI_AI_BASEURL を
// このサーバに向けるだけで AFM のベンチマークが取得できる。
//
// build: swiftc -target arm64-apple-macos27.0 -O main.swift -o afm_bridge
//

import Foundation
import Network
import FoundationModels

// MARK: - OpenAI wire format

struct ChatMessage: Decodable {
    let role: String
    let content: String?
}

struct ChatRequest: Decodable {
    let model: String?
    let messages: [ChatMessage]
    let temperature: Double?
    let max_tokens: Int?
}

// 変換に失敗した場合でもHTTP 200で返すためのセンチネル。
// sumibi_bench.py は APITimeoutError 以外の例外を捕捉せず実行全体が落ちるため、
// エラーを本文として返して計測を継続させる（結果JSONにそのまま残り、後から集計できる）。
enum Sentinel {
    static let guardrail = "[GUARDRAIL]"
    static let refusal = "[REFUSAL]"
    static let contextExceeded = "[CONTEXT_EXCEEDED]"
    static let error = "[ERROR]"
}

// MARK: - 設定

enum PromptMode: String {
    /// system -> Instructions, few-shot -> Transcript の prompt/response エントリ
    case transcript
    /// system -> Instructions, few-shot は1つのプロンプト文字列に連結
    case flat
}

struct Config {
    var port: UInt16 = 8765
    var permissiveGuardrails = true
    var verbose = false
    var promptMode: PromptMode = .transcript
}

func parseArgs() -> Config {
    var cfg = Config()
    var it = CommandLine.arguments.dropFirst().makeIterator()
    while let arg = it.next() {
        switch arg {
        case "--port":
            if let v = it.next(), let p = UInt16(v) { cfg.port = p }
        case "--strict-guardrails":
            cfg.permissiveGuardrails = false
        case "--permissive-guardrails":
            cfg.permissiveGuardrails = true
        case "--prompt-mode":
            if let v = it.next(), let m = PromptMode(rawValue: v) {
                cfg.promptMode = m
            } else {
                FileHandle.standardError.write("--prompt-mode must be transcript|flat\n".data(using: .utf8)!)
                exit(2)
            }
        case "--verbose":
            cfg.verbose = true
        case "--help", "-h":
            print("""
            usage: afm_bridge [--port N] [--strict-guardrails|--permissive-guardrails] [--verbose]

              --port N                  待ち受けポート (default: 8765)
              --permissive-guardrails   Guardrails.permissiveContentTransformations を使う (default)
              --strict-guardrails       Guardrails.default を使う
              --prompt-mode MODE        transcript (default) | flat
              --verbose                 リクエストごとの入出力を stderr に出す
            """)
            exit(0)
        default:
            FileHandle.standardError.write("unknown argument: \(arg)\n".data(using: .utf8)!)
            exit(2)
        }
    }
    return cfg
}

let config = parseArgs()

func log(_ s: String) {
    FileHandle.standardError.write((s + "\n").data(using: .utf8)!)
}

// MARK: - AFM 呼び出し

@available(macOS 27.0, *)
final class AFMEngine {
    let model: SystemLanguageModel
    let promptMode: PromptMode

    init(permissive: Bool, promptMode: PromptMode) {
        self.model = SystemLanguageModel(
            useCase: .general,
            guardrails: permissive ? .permissiveContentTransformations : .default
        )
        self.promptMode = promptMode
    }

    /// OpenAI形式の messages を FoundationModels の Transcript にマップして応答を得る。
    /// - system      -> Transcript.Instructions
    /// - 末尾以外の user/assistant -> Transcript.Prompt / Transcript.Response (few-shot)
    /// - 末尾の user -> 実際に応答させるプロンプト
    func respond(to req: ChatRequest) async -> (content: String, kind: String) {
        var systemParts: [String] = []
        var conversation: [ChatMessage] = []
        for m in req.messages {
            if m.role == "system" || m.role == "developer" {
                if let c = m.content { systemParts.append(c) }
            } else {
                conversation.append(m)
            }
        }

        guard let last = conversation.last, last.role == "user", let promptText = last.content else {
            return (Sentinel.error, "no-user-prompt")
        }
        let history = conversation.dropLast()

        var entries: [Transcript.Entry] = []
        if !systemParts.isEmpty {
            entries.append(.instructions(Transcript.Instructions(
                segments: [.text(Transcript.TextSegment(content: systemParts.joined()))],
                toolDefinitions: []
            )))
        }
        var finalPrompt = promptText
        switch promptMode {
        case .transcript:
            for m in history {
                guard let c = m.content else { continue }
                let seg: [Transcript.Segment] = [.text(Transcript.TextSegment(content: c))]
                if m.role == "user" {
                    entries.append(.prompt(Transcript.Prompt(segments: seg)))
                } else if m.role == "assistant" {
                    entries.append(.response(Transcript.Response(assetIDs: [], segments: seg)))
                }
            }
        case .flat:
            var lines: [String] = []
            for m in history {
                guard let c = m.content else { continue }
                lines.append(m.role == "user" ? "入力: \(c)" : "出力: \(c)")
            }
            lines.append("入力: \(promptText)")
            lines.append("出力:")
            finalPrompt = lines.joined(separator: "\n")
        }

        let session = LanguageModelSession(
            model: model,
            tools: [],
            transcript: Transcript(entries: entries)
        )

        var options = GenerationOptions(temperature: req.temperature)
        if let maxTokens = req.max_tokens {
            options.maximumResponseTokens = maxTokens
        }

        do {
            let response = try await session.respond(to: finalPrompt, options: options)
            return (response.content, "ok")
        } catch let error as LanguageModelError {
            switch error {
            case .guardrailViolation:
                log("  !! guardrail violation: \(promptText.prefix(60))")
                return (Sentinel.guardrail, "guardrail")
            case .refusal:
                log("  !! refusal: \(promptText.prefix(60))")
                return (Sentinel.refusal, "refusal")
            case .contextSizeExceeded:
                log("  !! context size exceeded")
                return (Sentinel.contextExceeded, "context")
            default:
                log("  !! LanguageModelError: \(error)")
                return (Sentinel.error, "error")
            }
        } catch {
            log("  !! error: \(error)")
            return (Sentinel.error, "error")
        }
    }
}

// MARK: - 集計カウンタ

final class Stats: @unchecked Sendable {
    private let lock = NSLock()
    private var counts: [String: Int] = [:]

    func record(_ kind: String) {
        lock.lock(); defer { lock.unlock() }
        counts[kind, default: 0] += 1
    }

    func summary() -> String {
        lock.lock(); defer { lock.unlock() }
        return counts.sorted { $0.key < $1.key }.map { "\($0.key)=\($0.value)" }.joined(separator: " ")
    }
}

let stats = Stats()

// MARK: - 最小HTTPサーバ

func httpResponse(status: String, json: Any) -> Data {
    let body = (try? JSONSerialization.data(withJSONObject: json)) ?? Data("{}".utf8)
    var head = "HTTP/1.1 \(status)\r\n"
    head += "Content-Type: application/json\r\n"
    head += "Content-Length: \(body.count)\r\n"
    head += "Connection: close\r\n\r\n"
    var data = Data(head.utf8)
    data.append(body)
    return data
}

func chatCompletionJSON(model: String, content: String) -> [String: Any] {
    return [
        "id": "chatcmpl-afm-\(UUID().uuidString.prefix(8))",
        "object": "chat.completion",
        "created": Int(Date().timeIntervalSince1970),
        "model": model,
        "choices": [[
            "index": 0,
            "message": ["role": "assistant", "content": content],
            "finish_reason": "stop",
        ]],
        // sumibi_bench.py は usage を使わないので 0 埋め
        "usage": ["prompt_tokens": 0, "completion_tokens": 0, "total_tokens": 0],
    ]
}

@available(macOS 27.0, *)
final class HTTPConnection {
    private let conn: NWConnection
    private let queue: DispatchQueue
    private let engine: AFMEngine
    private var buffer = Data()
    /// listener は接続を保持しないので、応答completionまで自分で自分を保持する
    private var retainSelf: HTTPConnection?

    init(conn: NWConnection, queue: DispatchQueue, engine: AFMEngine) {
        self.conn = conn
        self.queue = queue
        self.engine = engine
    }

    func start() {
        retainSelf = self
        conn.start(queue: queue)
        receive()
    }

    private func finish() {
        conn.cancel()
        retainSelf = nil
    }

    private func receive() {
        conn.receive(minimumIncompleteLength: 1, maximumLength: 1 << 20) { [weak self] data, _, isComplete, error in
            guard let self = self else { return }
            if let d = data, !d.isEmpty { self.buffer.append(d) }
            if let (head, body) = self.parseRequest() {
                self.handle(head: head, body: body)
            } else if isComplete || error != nil {
                self.finish()
            } else {
                self.receive()
            }
        }
    }

    /// ヘッダ終端と Content-Length 分の本文が揃ったらリクエストを返す
    private func parseRequest() -> (String, Data)? {
        let separator = Data("\r\n\r\n".utf8)
        guard let range = buffer.range(of: separator) else { return nil }
        let headData = buffer[buffer.startIndex..<range.lowerBound]
        guard let head = String(data: headData, encoding: .utf8) else { return nil }

        var contentLength = 0
        for line in head.split(separator: "\r\n") {
            let parts = line.split(separator: ":", maxSplits: 1)
            if parts.count == 2, parts[0].lowercased() == "content-length" {
                contentLength = Int(parts[1].trimmingCharacters(in: .whitespaces)) ?? 0
            }
        }
        let bodyStart = range.upperBound
        let available = buffer.distance(from: bodyStart, to: buffer.endIndex)
        guard available >= contentLength else { return nil }
        let bodyEnd = buffer.index(bodyStart, offsetBy: contentLength)
        return (head, Data(buffer[bodyStart..<bodyEnd]))
    }

    private func handle(head: String, body: Data) {
        let requestLine = head.split(separator: "\r\n").first.map(String.init) ?? ""
        let path = requestLine.split(separator: " ").dropFirst().first.map(String.init) ?? "/"

        if path.hasSuffix("/health") {
            send(httpResponse(status: "200 OK", json: ["status": "ok"]))
            return
        }
        guard path.hasSuffix("/chat/completions") else {
            send(httpResponse(status: "404 Not Found", json: ["error": ["message": "not found: \(path)"]]))
            return
        }

        guard let req = try? JSONDecoder().decode(ChatRequest.self, from: body) else {
            send(httpResponse(status: "400 Bad Request", json: ["error": ["message": "invalid request body"]]))
            return
        }

        Task { [engine] in
            let started = Date()
            let (content, kind) = await engine.respond(to: req)
            stats.record(kind)
            if config.verbose {
                let elapsed = String(format: "%.2f", Date().timeIntervalSince(started))
                log("[\(elapsed)s \(kind)] -> \(content)")
            }
            let json = chatCompletionJSON(model: req.model ?? "afm-3-core-advanced", content: content)
            self.send(httpResponse(status: "200 OK", json: json))
        }
    }

    private func send(_ data: Data) {
        conn.send(content: data, completion: .contentProcessed { [self] _ in
            finish()
        })
    }
}

// MARK: - 起動

@available(macOS 27.0, *)
func run() throws {
    let probe = SystemLanguageModel.default
    guard probe.isAvailable else {
        log("AFM は利用できません: \(probe.availability)")
        exit(1)
    }

    let engine = AFMEngine(permissive: config.permissiveGuardrails, promptMode: config.promptMode)
    log("model variant  : \(engine.model.variant.displayName)")
    log("context size   : \(engine.model.contextSize)")
    log("guardrails     : \(config.permissiveGuardrails ? "permissiveContentTransformations" : "default")")
    log("prompt mode    : \(config.promptMode.rawValue)")
    log("supports ja_JP : \(engine.model.supportsLocale(Locale(identifier: "ja_JP")))")

    let queue = DispatchQueue(label: "afm-bridge", attributes: .concurrent)
    let listener = try NWListener(using: .tcp, on: NWEndpoint.Port(rawValue: config.port)!)
    listener.newConnectionHandler = { conn in
        HTTPConnection(conn: conn, queue: queue, engine: engine).start()
    }
    listener.stateUpdateHandler = { state in
        if case .ready = state {
            log("listening on http://127.0.0.1:\(config.port)  (SUMIBI_AI_BASEURL=http://127.0.0.1:\(config.port))")
        }
    }
    listener.start(queue: queue)

    // Ctrl-C で集計を出して終了
    let sigintSource = DispatchSource.makeSignalSource(signal: SIGINT, queue: .main)
    sigintSource.setEventHandler {
        log("\nrequests: \(stats.summary())")
        exit(0)
    }
    sigintSource.resume()
    signal(SIGINT, SIG_IGN)

    dispatchMain()
}

if #available(macOS 27.0, *) {
    try run()
} else {
    log("macOS 27.0 以降が必要です")
    exit(1)
}
