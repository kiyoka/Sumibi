# 安全

## API Key 的安全管理

### 概述

Sumibi 使用 OpenAI、Google Gemini 及其他 LLM 服务的 API 来实现输入法功能。使用这些服务需要 API Key，API Key 属于 **机密信息**，需要妥善管理。

从 5.0.0 版本开始，Sumibi 支持以下 3 种安全保存 API Key 的方式：

1. **环境变量**（默认）- 传统方式
2. **GPG 加密文件** - 用密码保护的加密文件
3. **macOS Keychain** - macOS 标准的安全存储

### 为什么 API Key 的安全管理很重要

API Key 泄露可能导致以下风险：

- **被盗用产生高额费用** - 第三方使用您的 API Key 进行大量 API 调用
- **账号被封禁** - 因违反使用条款导致 API 访问被停止
- **数据泄露** - 您的输入历史可能被第三方读取

以下情况 **强烈建议使用环境变量以外的方式**：

- dotfiles 公开在 GitHub 等平台上
- 多人共用同一台机器
- 需要更高的安全级别

## 3 种管理方式的比较

| 方式 | 安全性 | 配置难度 | 支持的操作系统 | 推荐度 |
|------|--------|---------|--------------|--------|
| 环境变量 | 低 | 简单 | 全部 | 个人使用 |
| GPG 加密 | 高 | 中等 | Linux/macOS/Windows | 推荐 |
| macOS Keychain | 高 | 简单 | 仅 macOS | macOS 推荐 |

## 选择配置方式

请选择您想使用的方式，查看详细的配置步骤：

### [方式 1：环境变量（默认）](SECURITY_ENVIRONMENT.zh.md)

传统方式。最简单，但安全级别较低。

- 配置简单
- 支持所有操作系统
- API Key 以明文保存

**-> [查看环境变量配置步骤](SECURITY_ENVIRONMENT.zh.md)**

---

### [方式 2：GPG 加密文件](SECURITY_GPG.zh.md)

将 API Key 保存在受密码保护的加密文件中。

- API Key 经过密码加密
- 即使公开 dotfiles 也安全
- 支持所有操作系统

**-> [查看 GPG 加密配置步骤](SECURITY_GPG.zh.md)**

---

### [方式 3：macOS Keychain](SECURITY_KEYCHAIN.zh.md)

将 API Key 保存在 macOS 标准的 Keychain 中。

- 使用 macOS 标准的安全存储
- 无需输入密码（或使用 Touch ID 认证）
- 仅在 macOS 上可用

**-> [查看 macOS Keychain 配置步骤](SECURITY_KEYCHAIN.zh.md)**

---

## 安全最佳实践

### 推荐事项

1. **使用 GPG 或 Keychain**
   - 比环境变量更安全

2. **定期更换 API Key**
   - 每 3-6 个月生成新的 API Key

3. **及时停用不再需要的 API Key**
   - 在 OpenAI 或 Gemini 的管理页面删除旧 Key

4. **公开 dotfiles 时不要使用环境变量**
   - 请使用 GPG 或 Keychain

5. **监控 API Key 的使用量**
   - 以便及早发现异常使用

### 应避免的事项

1. **不要将 API Key 写入源代码**
   - 一旦提交到 Git，会保留在历史记录中

2. **不要以明文保存 API Key**
   - 请使用安全的方式保存

3. **不要通过邮件或聊天发送 API Key**
   - 请使用加密的方式分享

4. **不要将 API Key 提交到公开仓库**
   - 使用 `.gitignore` 排除

---

## 参考资料

- [GnuPG 官方网站](https://gnupg.org/)
- [Emacs auth-source 文档](https://www.gnu.org/software/emacs/manual/html_node/auth/)
- [OpenAI API Keys 管理](https://platform.openai.com/api-keys)
- [Google AI Studio](https://makersuite.google.com/app/apikey)

---

## 报告安全漏洞

如果您发现 Sumibi 存在安全问题，请不要使用 GitHub Issues，而是直接联系维护者：

- 仓库：https://github.com/kiyoka/Sumibi
- 安全政策：使用 GitHub Security Advisory
