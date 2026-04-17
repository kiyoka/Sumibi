# Sumibi Chinese

基于 LLM API 的 Emacs 中文拼音输入法

## 什么是 Sumibi Chinese

这是一个为 Emacs 设计的中文输入系统，是 [Sumibi](README.en.md)（日语输入法）的中文版模块。

Sumibi Chinese 是无模式的。
无需切换到中文输入模式即可输入中文。

通过输入拼音并按 Ctrl-J，即可将拼音转换为中文（简体字）。

常见问题请参阅 [FAQ](FAQ.zh.md)。

## 支持的 Emacs 版本

Emacs version 29.x 以上（Windows/Linux/macOS）。需要先安装 Sumibi 主包。

## 安装

1. 注册 OpenAI AI 的订阅服务。

[https://platform.openai.com/account/api-keys](https://platform.openai.com/account/api-keys)

2. 将 OpenAI API Key 设置到环境变量 `OPENAI_API_KEY`。（也可以使用 `SUMIBI_AI_API_KEY`）
3. 从 MELPA 安装包「sumibi」。
4. 在 ~/.emacs.d/init.el 中添加以下代码。

```lisp
(require 'sumibi-chinese)
(global-sumibi-chinese-mode 1)
```

如果需要输出繁体字（台湾、香港等地区），请添加以下设置：

```lisp
(require 'sumibi-chinese)
(setq sumibi-chinese-character-set 'traditional)
(global-sumibi-chinese-mode 1)
```

使用 Gemini API 的情况下，设置环境变量 `GEMINI_API_KEY`，并在 init.el 中添加以下内容：

```lisp
(require 'sumibi-chinese)
(setq sumibi-provider 'gemini)
(global-sumibi-chinese-mode 1)
```

## 确认安装是否成功

重启 Emacs 后，状态栏会显示 `[简]`（简体字模式）或 `[繁]`（繁体字模式）。

## 将拼音转换为中文

1. 将光标移到拼音文本的末尾，按 Ctrl-J 即可转换为中文。

   例：
   ```
   wo shi zhongguo ren  →  我是中国人 (简体) / 我是中國人 (繁体)
   ni hao ma            →  你好吗 (简体) / 你好嗎 (繁体)
   jin tian tian qi hen hao  →  今天天气很好 (简体) / 今天天氣很好 (繁体)
   xue xi zhong wen     →  学习中文 (简体) / 學習中文 (繁体)
   ```

2. 如果对转换结果不满意，再次按 Ctrl-J 会弹出候选列表，可以从中选择。

3. 也可以选中一段拼音文本（region），然后按 Ctrl-J 进行转换。

### 声调数字输入

在拼音音节后加上声调数字可以提高转换精度，尤其是同音字较多的情况下。

| 数字 | 声调 | 示例 |
|------|------|------|
| 1 | 第一声（阴平） | mā → `ma1` |
| 2 | 第二声（阳平） | má → `ma2` |
| 3 | 第三声（上声） | mǎ → `ma3` |
| 4 | 第四声（去声） | mà → `ma4` |

转换示例：

```
ma1 ma2 ma3 ma4  →  妈麻马骂
wo3 xiang3 he1 yi4 bei1 ka1 fei1  →  我想喝一杯咖啡
```

- 声调数字为可选项，不加声调也可以正常使用
- 声调数字和无声调的拼音可以混合使用（例：`wo shi zhongguo2 ren2`）

## 简体字/繁体字切换

默认输出简体字。如果需要繁体字输出，请在 init.el 中设置：

```lisp
;; 繁体字模式
(setq sumibi-chinese-character-set 'traditional)
```

切换回简体字：

```lisp
;; 简体字模式（默认）
(setq sumibi-chinese-character-set 'simplified)
```

也可以通过 `M-x customize-variable RET sumibi-chinese-character-set` 进行交互式设置。

## 撤销

如果对转换结果不满意，按 ESC-u 键可以撤销。

或者，在候选列表中选择「原文」选项，恢复原始拼音。

## 不想将 API Key 设置为环境变量？

Sumibi 支持 3 种 API Key 安全管理方式：

1. **环境变量**（默认）- 传统方式
2. **GPG 加密文件** - 用密码保护的加密文件
3. **macOS Keychain** - macOS 标准的安全存储

详细配置方法请参阅[安全指南](SECURITY.zh.md)。

## 切换 AI 服务

- 切换到 Gemini（推荐）

    ```lisp
    (setq sumibi-provider 'gemini)
    ```

- 切换到 DeepSeek

    ```lisp
    ;; 设置环境变量 DEEPSEEK_API_KEY
    (setq sumibi-provider 'deepseek)
    ```

- 切换到本地 LLM（LM Studio）

    ```lisp
    ;; 在 LM Studio 中加载模型并启动服务器
    (setq sumibi-provider 'local)
    ```

    默认连接 `http://127.0.0.1:1234/v1`，使用模型 `google/gemma-4-e4b`。无需 API Key。

    LM Studio 的「Enable Thinking」设置因模型而异，推荐如下：

    | 模型 | Enable Thinking | 说明 |
    |------|----------------|------|
    | `google/gemma-4-e4b` | ON | 启用 Thinking 可提高转换精度 |
    | `google/gemma-4-26b-a4b` | OFF | 无需 Thinking 即有足够精度，响应速度更快 |
    | `mlx-community/gemma-4-26b-a4b-it` | OFF | 面向 Apple Silicon 的 MLX 优化版本。比 `google/gemma-4-26b-a4b` 快约 2.5 倍（中位数 2.12 秒） |

    也可以使用传统的环境变量方式：

    ```
    (setenv "SUMIBI_AI_API_KEY" "xxxxxxxx") ;; 虚拟 API Key
    (setenv "SUMIBI_AI_BASEURL" "http://127.0.0.1:1234/") ;; 本地 LLM 端点 URL
    (setenv "SUMIBI_AI_MODEL" "google/gemma-4-e4b") ;; 本地 LLM 模型名
    ```

## 与日语版 Sumibi 的并用

Sumibi Chinese 和日语版 Sumibi 可以同时使用，但由于两者都绑定了 C-j 键，建议根据需要修改键绑定：

```lisp
;; 日语版使用 C-j，中文版使用 C-; 等
(require 'sumibi)
(global-sumibi-mode 1)

(require 'sumibi-chinese)
(define-key sumibi-chinese-mode-map (kbd "C-;") 'sumibi-chinese-trans)
(global-sumibi-chinese-mode 1)
```

## 在无法使用 LLM 的环境中

本输入法需要 LLM API 连接。如果无法使用 LLM，建议考虑其他离线中文输入法，如 [pyim](https://github.com/tumashu/pyim)。
