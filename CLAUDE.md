
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## プロジェクト概要

Sumibiは、AI (ChatGPT API) を使用したEmacs用の日本語入力メソッドです。モードレス入力が特徴で、入力モードの切り替えなしに日本語を入力できます。

## 開発コマンド

### ビルドとリリース
```bash
# リリースアーカイブ (tar.gz) の作成
make release

# 生成物のクリーンアップ
make clean
```

### テスト
```bash
# ERT (Emacs Regression Testing) テストの実行
make test
```

### リント／構文チェック
Emacs Lispファイルを編集した後は、**必ず**括弧バランスチェックツールを実行してください：

```bash
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし括弧の不整合が検出されたら：
1. 他の編集作業はせず、指摘された行番号の括弧を修正
2. 再度 `agent-lisp-paren-aid-linux` を実行して確認
3. すべての括弧が整合してから次の作業へ

**重要**: LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

### GitHub連携

ghコマンドはインストールされていませんので、issueの内容を確認する時は、以下のようなURLを直接開いてください。
https://github.com/kiyoka/Sumibi/issues/53

## アーキテクチャ

### ディレクトリ構造
- `lisp/` - メインのEmacs Lispソースコード
  - `sumibi.el` - メイン実装 (v3.5.0)
  - `sumibi-localdic.el` - ローカル辞書サポート
- `test/` - ERTテストファイル
- `benchmark/` - パフォーマンスベンチマークツール
- `skkdic/` - SKK辞書関連ファイル

### 主要コンポーネント
1. **AI変換エンジン**: OpenAI/Gemini/DeepSeek APIを使用してローマ字を日本語に変換
2. **Mozcバックエンド**: オフライン環境用のフォールバック
3. **ポップアップUI**: 候補選択インターフェース
4. **履歴管理**: より良いコンテキスト理解のための変換履歴

### 依存関係
- Emacs >= 29.0
- popup >= 0.5.9
- unicode-escape >= 1.1
- deferred >= 0.5.1
- mozc (オプション)

## 編集プロセス

- sumibi.el を編集した後は、必ず agent-lisp-paren-aid-linux を実行して、閉じ括弧が合っているか確認してください。

もし括弧が整合していない場合は、修正すべき行番号を教えてくれます。

```
agent-lisp-paren-aid-linux lisp/sumibi.el
```

もし不整合が検出されたら他の編集作業はせず、一旦指摘された行番号に括弧を補う修正のみを行って、
再度 agent-lisp-paren-aid-linux を実行するようにしてください。
LLMはLisp括弧を数えるのが苦手なため、自分で数えたり考えたりせず、必ずこのツールを使うようにしてください。

## 外部ライブラリ

もし、ちょっとした手元のテストコードで以下のライブラリを読み込みたくなった場合は、 ~/.emacs.d/elpa/ から検索してパスを追加してください

(require 'cl-lib)
(require 'popup)
(require 'url)
(require 'url-http)
(require 'unicode-escape)
(require 'deferred)
(require 'sumibi-localdic)

## KKCコマンドラインツールの調査結果

**調査日**: 2025-10-07

### 結論
KKCは漢字に変換済みの文章を含めて変換可能であることを確認しました。

### テスト結果

1. **純粋なひらがな入力**:
```bash
$ echo "ぜいきんをおさめる" | kkc decoder
0: <税金/ぜいきん><を/を><収め/おさめ><る/る>
```

2. **漢字とひらがなの混在入力**:
```bash
$ echo "税金をおさめる" | kkc decoder
0: <税金/税金><を/を><収め/おさめ><る/る>
```
→ 漢字部分はそのまま保持され、ひらがな部分のみが変換される

3. **複数候補の取得**:
```bash
$ echo "税金をおさめる 5" | kkc decoder
0: <税金/税金><を/を><収め/おさめ><る/る>
1: <税金/税金><を/を><治め/おさめ><る/る>
2: <税金/税金><を/を><納め/おさめ><る/る>
3: <税金/税金><を/を><修め/おさめ><る/る>
4: <税金/税金><を/を><おさめ/おさめ><る/る>
```

### Issue #92の実装方針

1. カーソル位置前方の文脈（漢字を含む）を取得
2. Mozcで得た変換候補のひらがな部分を文脈に追加
3. `echo "文脈+ひらがな N" | kkc decoder` でKKC変換を実行
4. KKCの最上位候補をMozc候補リストの先頭に昇格させる

github issue 92を読んで内容を理解してください。
https://github.com/kiyoka/Sumibi/issues/92


それでは実装してください。既存実装には、kkc利用した部分はありません。
kkcをコマンドラインツールとして呼び出し、mozcの変換候補の並び替えに利用する実装を行ってください。
そのためには、変換対象の周辺文脈をkkcに入れると同時に、mozcが返す変換候補から何が適切かを並び替えする実装となります。


再度、シンプルな実装を試みてください。

続きを実装してください。

一旦括弧のバランスは無視して、実装したい意味だけを実装してみてください。
  * 


----- memo ----

● Issue #92の実装完了

  実装内容

  1. 新しいファイル lisp/sumibi-kkc.elを作成:
    - KKC統合のための専用モジュール
    - 3つのカスタム変数:
        - sumibi-kkc-enabled: KKC機能の有効/無効
      - sumibi-kkc-command: KKCコマンドのパス
      - sumibi-kkc-context-chars: 文脈として使用する文字数
    - 3つの関数:
        - sumibi-kkc--call: KKCコマンドを呼び出し
      - sumibi-kkc--get-context: カーソル前の文脈を取得
      - sumibi-kkc-reorder-candidates: Mozc候補をKKCで並び替え
  2. lisp/sumibi.elへの統合:
    - (require 'sumibi-kkc)を追加
    - 既に108行目でsumibi-kkc-reorder-candidatesを呼び出す実装済み

  動作

  - デフォルトではsumibi-kkc-enabledがnilなので、KKCは使用されません
  - ユーザーが(setq sumibi-kkc-enabled t)を設定すると、KKCによる候補並び替えが有効になります
  - KKCは:
    a. カーソル前の日本語文脈を取得
    b. 変換対象のひらがなと組み合わせてKKCに渡す
    c. KKCの最上位候補がMozc候補リストに含まれていれば、それを先頭に移動

  テスト結果

  - ✅ 括弧バランスチェック: OK (sumibi.el, sumibi-kkc.el両方)
  - ✅ 全テスト (23件): PASS


-----

sumibi-kkc-enabledは廃止し、
カスタマイズ変数 sumibi-backend に3つ目の選択肢を追加してください。
"Mozc+kkc (local)"という選択肢を追加してください。
