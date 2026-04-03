# 通过环境变量管理 API Key

[<- 返回安全指南首页](SECURITY.zh.md)

## 概述

使用环境变量管理 API Key 是最简单的方式。只需在配置文件（`~/.bashrc` 或 `~/.zshrc`）中添加环境变量即可。

**安全级别：** 低
**配置难度：** 简单
**支持的操作系统：** 全部

## 优缺点

### 优点

- **配置简单** - 只需在 shell 配置文件中添加一行
- **支持所有操作系统** - Linux、macOS、Windows（WSL）均可使用
- **无需额外工具** - 不需要安装 GPG 或配置 Keychain

### 缺点

- **API Key 以明文保存** - 未经加密直接记录在文件中
- **公开 dotfiles 会导致 API Key 泄露** - 如果将 dotfiles 发布到 GitHub 等平台会很危险
- **可能通过进程列表看到 API Key** - `ps` 命令等可能显示环境变量

## 配置步骤

### 1. 设置环境变量

在您使用的 shell 配置文件中添加环境变量。

#### 使用 bash 的情况

编辑 `~/.bashrc` 或 `~/.bash_profile`：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
# 或者
export OPENAI_API_KEY="sk-your-api-key-here"
```

#### 使用 zsh 的情况（macOS 默认）

编辑 `~/.zshrc`：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
# 或者
export OPENAI_API_KEY="sk-your-api-key-here"
```

#### 使用 fish 的情况

编辑 `~/.config/fish/config.fish`：

```fish
set -x SUMIBI_AI_API_KEY "sk-your-api-key-here"
# 或者
set -x OPENAI_API_KEY "sk-your-api-key-here"
```

**注意：** 请替换为您的实际 API Key。

### 2. 使配置生效

编辑配置文件后，运行以下命令使配置生效：

```bash
# bash 的情况
source ~/.bashrc

# zsh 的情况
source ~/.zshrc

# fish 的情况
source ~/.config/fish/config.fish
```

或者重新启动终端。

### 3. 确认环境变量已设置

```bash
echo $SUMIBI_AI_API_KEY
# 或者
echo $OPENAI_API_KEY
```

如果显示了 API Key，说明设置完成。

### 4. Emacs 配置（可选）

无需特别配置（这是默认行为）。

如需明确指定，可在 `~/.emacs.d/init.el` 中添加：

```elisp
(setq sumibi-api-key-source 'environment)
```

## 使用多个 API 服务的情况

如需同时使用 OpenAI 和 Gemini 等多个服务，可以分别设置环境变量：

```bash
# OpenAI
export OPENAI_API_KEY="sk-openai-key-here"

# Gemini
export GEMINI_API_KEY="your-gemini-key-here"
```

Sumibi 目前支持 `SUMIBI_AI_API_KEY` 或 `OPENAI_API_KEY`。

## 故障排除

### 错误："API Key 未找到"

**原因：** 环境变量未设置

**解决方法：**

1. **确认环境变量是否已设置：**

```bash
echo $SUMIBI_AI_API_KEY
echo $OPENAI_API_KEY
```

如果没有任何输出，说明环境变量未设置。

2. **检查配置文件：**

打开配置文件（`~/.bashrc` 或 `~/.zshrc`），确认是否有 `export` 行。

3. **使配置生效：**

```bash
source ~/.bashrc  # 或 source ~/.zshrc
```

4. **重启 Emacs：**

更改环境变量后，需要重启 Emacs。

### Emacs 无法读取环境变量

**原因：** 以 GUI 应用方式启动 Emacs 时，可能不会继承 shell 的环境变量（特别是 macOS）

**解决方法 1：** 从终端启动 Emacs

```bash
emacs
```

**解决方法 2：** 使用 `exec-path-from-shell` 包

在 `~/.emacs.d/init.el` 中添加：

```elisp
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SUMIBI_AI_API_KEY")
    (exec-path-from-shell-copy-env "OPENAI_API_KEY")))
```

### 公开 dotfiles 时的注意事项

**问题：** 将 dotfiles 发布到 GitHub 等平台时，API Key 也会被公开

**解决方法 1：** 将敏感信息分离到单独文件

在 `~/.bashrc` 或 `~/.zshrc` 中添加：

```bash
# 将敏感信息分离到单独文件
if [ -f ~/.secrets ]; then
    source ~/.secrets
fi
```

创建 `~/.secrets` 文件（添加到 `.gitignore`）：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
```

在 `.gitignore` 中添加：

```
.secrets
```

**解决方法 2：** 使用 GPG 或 Keychain

使用比环境变量更安全的方式：

- [使用 GPG 加密文件](SECURITY_GPG.zh.md)
- [使用 macOS Keychain](SECURITY_KEYCHAIN.zh.md)（仅 macOS）

## 安全注意事项

### 重要提示

1. **公开 dotfiles 时，请将 API Key 分离到单独文件**
   - 发布到 GitHub 等平台时需特别注意

2. **不要在共享机器上使用**
   - 其他用户可能看到环境变量

3. **定期更换 API Key**
   - 每 3-6 个月生成新的 API Key

4. **停用旧的 API Key**
   - 在 OpenAI 或 Gemini 的管理页面删除

## 迁移到更安全的方式

建议考虑迁移到比环境变量更安全的方式：

- **[GPG 加密文件](SECURITY_GPG.zh.md)** - 用密码保护的加密文件（推荐）
- **[macOS Keychain](SECURITY_KEYCHAIN.zh.md)** - macOS 标准的安全存储（仅 macOS）

---

[<- 返回安全指南首页](SECURITY.zh.md)
