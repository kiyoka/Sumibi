# FAQ 常见问题

## Q. Sumibi Chinese 的特点是什么？

A. 这是一个无模式的中文拼音输入系统。

Windows 和 macOS 的标准中文输入法需要切换输入模式，而 Sumibi Chinese 无论在什么场景下，只需按 Ctrl-J 即可将拼音转换为中文。

## Q. 支持哪些操作系统？

A. 支持 Windows/Linux/macOS。

## Q. 支持哪些 Emacs 版本？

A. 支持 Emacs 29.x 以上版本。

## Q. 除了 Emacs 还需要其他软件吗？

A. 不需要。但需要先安装 Sumibi 主包。

## Q. 需要 OpenAI 的账号吗？

A. 是的。需要订阅 OpenAI API 并获取 API Key。也可以使用 Gemini API（推荐）。

## Q. 使用 Sumibi Chinese 的费用大概是多少？

A. 使用 gpt-3.5-turbo 模型时，编写一篇文档大约需要 0.01-0.05 元人民币。如果使用 GPT-4 模型，费用大约是 20-30 倍。建议日常使用 Gemini（免费额度充足），需要高精度时切换到 GPT-4 系列。

## Q. 转换结果不准确，有什么技巧吗？

A. 建议尽量输入较长的拼音句子后再进行转换，LLM 可以利用更多上下文来提高准确性。

如果转换后某个字不正确，可以选中该部分后按 Ctrl-J，会显示候选列表供您选择。

## Q. 可以使用 GPT-4 以外的模型吗？

A. 可以。请修改自定义变量 `sumibi-current-model`，或通过 `M-x sumibi-switch-model` 动态切换。

## Q. OpenAI 的响应太慢，可以设置超时吗？

A. 可以。请修改自定义变量 `sumibi-api-timeout`（默认 60 秒）。如果经常翻译长文，建议增大到 300 秒。

## Q. 可以同时使用日语版 Sumibi 吗？

A. 可以。但由于两者都绑定了 C-j 键，建议修改其中一个的键绑定：

```lisp
;; 日语版使用 C-j，中文版使用 C-;
(require 'sumibi)
(global-sumibi-mode 1)

(require 'sumibi-chinese)
(define-key sumibi-chinese-mode-map (kbd "C-;") 'sumibi-chinese-trans)
(global-sumibi-chinese-mode 1)
```

## Q. 可以输出繁体字吗？

A. 可以。在 init.el 中添加以下设置即可切换为繁体字输出：

```lisp
(setq sumibi-chinese-character-set 'traditional)
```

设置后状态栏会从 `[简]` 变为 `[繁]`。

## Q. 不想将 API Key 设置为环境变量，还有其他方法吗？

A. Sumibi 支持 3 种 API Key 管理方式：

1. **环境变量**（默认）- 最简单但安全性较低
2. **GPG 加密文件** - 用密码保护的加密文件
3. **macOS Keychain** - macOS 标准的安全存储

详情请参阅[安全指南](SECURITY.zh.md)。
