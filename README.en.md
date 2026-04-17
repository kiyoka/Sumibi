# Sumibi Chinese

Chinese Pinyin input method for Emacs powered by LLM API

## What is Sumibi Chinese

A Chinese input system for Emacs. This is the Chinese module of [Sumibi](README.md) (Japanese input method).

Sumibi Chinese is modeless.
You can type Chinese without switching to a Chinese input mode.

Simply type pinyin and press Ctrl-J to convert it to Chinese. Both Simplified and Traditional Chinese are supported.

## Supported Emacs Versions

Emacs version 29.x or later (Windows/Linux/macOS). The main Sumibi package is required.

## Installation

1. Subscribe to OpenAI AI.

[https://platform.openai.com/account/api-keys](https://platform.openai.com/account/api-keys)

2. Set your OpenAI API key to the environment variable `OPENAI_API_KEY`. (`SUMIBI_AI_API_KEY` can also be used.)
3. Install the "sumibi" package from MELPA.
4. Add the following code to ~/.emacs.d/init.el.

```lisp
(require 'sumibi-chinese)
(global-sumibi-chinese-mode 1)
```

For Traditional Chinese output (used in Taiwan, Hong Kong, etc.), add the following setting:

```lisp
(require 'sumibi-chinese)
(setq sumibi-chinese-character-set 'traditional)
(global-sumibi-chinese-mode 1)
```

To use Gemini API, set the environment variable `GEMINI_API_KEY` and add the following to init.el:

```lisp
(require 'sumibi-chinese)
(setq sumibi-provider 'gemini)
(global-sumibi-chinese-mode 1)
```

## Verifying Successful Installation

After restarting Emacs, `[简]` (Simplified) or `[繁]` (Traditional) will appear in the status bar.

## Converting Pinyin to Chinese

1. Place the cursor at the end of the pinyin text and press Ctrl-J to convert it to Chinese.

   Examples:
   ```
   wo shi zhongguo ren  →  我是中国人 (Simplified) / 我是中國人 (Traditional)
   ni hao ma            →  你好吗 (Simplified) / 你好嗎 (Traditional)
   jin tian tian qi hen hao  →  今天天气很好 (Simplified) / 今天天氣很好 (Traditional)
   xue xi zhong wen     →  学习中文 (Simplified) / 學習中文 (Traditional)
   ```

2. If you are not satisfied with the conversion result, press Ctrl-J again to display a popup of candidates to choose from.

3. You can also select a region of pinyin text and press Ctrl-J to convert it.

### Tone Number Input

You can append tone numbers after pinyin syllables for improved accuracy, especially when there are many homophones.

| Number | Tone | Example |
|--------|------|---------|
| 1 | 1st tone (high level) | mā → `ma1` |
| 2 | 2nd tone (rising) | má → `ma2` |
| 3 | 3rd tone (dipping) | mǎ → `ma3` |
| 4 | 4th tone (falling) | mà → `ma4` |

Examples:

```
ma1 ma2 ma3 ma4  →  妈麻马骂
wo3 xiang3 he1 yi4 bei1 ka1 fei1  →  我想喝一杯咖啡
```

- Tone numbers are optional — pinyin without tones works as before
- You can mix toned and toneless pinyin (e.g., `wo shi zhongguo2 ren2`)

## Simplified / Traditional Chinese

The default output is Simplified Chinese. To switch to Traditional Chinese, add the following to init.el:

```lisp
;; Traditional Chinese mode
(setq sumibi-chinese-character-set 'traditional)
```

To switch back to Simplified Chinese:

```lisp
;; Simplified Chinese mode (default)
(setq sumibi-chinese-character-set 'simplified)
```

You can also use `M-x customize-variable RET sumibi-chinese-character-set` for interactive configuration.

## Undo

If you are not satisfied with the conversion result, press ESC-u to undo.

Alternatively, you can select the "原文" (original text) option from the candidate list to restore the original pinyin.

## Switching AI Services

The method for switching AI services is the same as the Japanese version of Sumibi. See the "利用するAIサービスの切り替え" section in [README](README.md) for details.

- Switch to Gemini (recommended)

    ```lisp
    (setq sumibi-provider 'gemini)
    ```

- Switch to DeepSeek

    ```lisp
    ;; Set the environment variable DEEPSEEK_API_KEY
    (setq sumibi-provider 'deepseek)
    ```

- Switch to local LLM (LM Studio)

    ```lisp
    ;; Start LM Studio server with a model loaded
    (setq sumibi-provider 'local)
    ```

    By default, it connects to `http://127.0.0.1:1234/v1` and uses the model `google/gemma-4-e4b`. No API Key is required.

    The recommended "Enable Thinking" setting in LM Studio varies by model:

    | Model | Enable Thinking | Notes |
    |-------|----------------|-------|
    | `google/gemma-4-e4b` | ON | Thinking improves conversion accuracy |
    | `google/gemma-4-26b-a4b` | OFF | Sufficient accuracy without Thinking, faster response |
    | `mlx-community/gemma-4-26b-a4b-it` | OFF | MLX-optimized build for Apple Silicon. ~2.5x faster than `google/gemma-4-26b-a4b` (median 2.12s) |

    The legacy environment variable method is also available:

    ```
    (setenv "SUMIBI_AI_API_KEY" "xxxxxxxx") ;; Dummy API key
    (setenv "SUMIBI_AI_BASEURL" "http://127.0.0.1:1234/") ;; Local LLM endpoint URL
    (setenv "SUMIBI_AI_MODEL" "google/gemma-4-e4b") ;; Local LLM model name
    ```

## Using Together with Japanese Sumibi

Sumibi Chinese and the Japanese version of Sumibi can be used simultaneously. However, since both bind to the C-j key, you may want to change the keybinding as needed:

```lisp
;; Japanese version uses C-j, Chinese version uses C-;
(require 'sumibi)
(global-sumibi-mode 1)

(require 'sumibi-chinese)
(define-key sumibi-chinese-mode-map (kbd "C-;") 'sumibi-chinese-trans)
(global-sumibi-chinese-mode 1)
```

## Environments Without LLM Access

This input method requires an LLM API connection. If you cannot use an LLM, consider other offline Chinese input methods such as [pyim](https://github.com/tumashu/pyim).
