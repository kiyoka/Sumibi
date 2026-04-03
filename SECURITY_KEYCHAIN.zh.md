# 通过 macOS Keychain 管理 API Key

[<- 返回安全指南首页](SECURITY.zh.md)

## 概述

将 API Key 保存在 macOS 标准的 Keychain 中。虽然仅在 macOS 上可用，但这是最集成化的安全方式。可以充分利用 Touch ID 认证和密码自动化等 macOS 功能。

**安全级别：** 高
**配置难度：** 简单
**支持的操作系统：** 仅 macOS

## 优缺点

### 优点

- **macOS 标准的安全存储** - Apple 提供的高可靠性安全机制
- **无需输入密码** - 或使用 Touch ID 认证（取决于 Mac 的设置）
- **配置相对简单** - 可以通过命令行简单配置
- **可与其他应用共享 Keychain** - 与 macOS 的其他应用集成

### 缺点

- **仅在 macOS 上可用** - 无法在 Linux 或 Windows 上使用
- **更换机器时需要重新配置** - Keychain 基本上是机器专属的

## 所需条件

- macOS

## 配置步骤

可以直接在 Emacs 中完成配置，无需命令行操作。

#### 1. Emacs 配置

在 `~/.emacs.d/init.el` 中添加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

#### 2. 保存 API Key

在 Emacs 中执行以下命令：

```
M-x sumibi-setup-api-key
```

会显示提示，请输入 API Key：

```
请输入 API Key (host: api.openai.com):
```

**注意：** `api.openai.com` 部分会根据环境变量 `SUMIBI_AI_BASEURL` 的设置而变化。

保存成功后，会显示以下信息：

```
API Key 已保存到 Keychain (host: api.openai.com)
```

#### 3. 确认

保存后重启 Emacs，Sumibi 会自动从 Keychain 获取 API Key 并运行。

**提示：** 首次启动时如果 API Key 未设置，会自动显示保存提示。

## 使用时的行为

启动 Emacs 使用 Sumibi 时，macOS 会自动从 Keychain 获取 API Key。

Emacs 首次访问 Keychain 项目时，会显示以下对话框：

```
"Emacs" 正在尝试访问机密信息。
请输入 "login" Keychain 的密码。
```

- **"允许"** - 仅本次允许
- **"始终允许"** - 以后始终允许（推荐）

**注意：**
- 选择 "**始终允许**" 后，之后（包括 macOS 重启后）不会再显示该对话框
- 登录 macOS 后，登录 Keychain 会自动解锁，因此 macOS 重启后无需特别操作
- 如果仅选择 "允许"，每次启动 Emacs 时都会显示该对话框

## 使用多个 API 服务的情况

如需同时使用 OpenAI 和 Gemini 等多个服务，可以切换环境变量 `SUMIBI_AI_BASEURL`，分别注册各服务的 API Key。

### 注册 OpenAI

使用默认配置（`SUMIBI_AI_BASEURL` 未设置）执行 `M-x sumibi-setup-api-key`：

```
请输入 API Key (host: api.openai.com):
```

### 注册 Google Gemini

在 `~/.zshrc` 或 `~/.bashrc` 中添加：

```bash
export SUMIBI_AI_BASEURL="https://generativelanguage.googleapis.com/v1beta/openai/"
```

重启终端后，在 Emacs 中执行 `M-x sumibi-setup-api-key`：

```
请输入 API Key (host: generativelanguage.googleapis.com):
```

**注意：**
- 请确认提示中显示的主机名，输入对应服务的 API Key
- 更改环境变量后，需要重启终端或 Emacs

## 故障排除

### 错误："macOS Keychain 仅在 macOS 上可用"

**原因：** 在 macOS 以外的操作系统上尝试使用 `auth-source-keychain`

**解决方法：**

- 如果确实在使用 macOS，请确认 Emacs 是否正确识别了操作系统：

```elisp
;; 在 Emacs 中执行以下代码
(message "%s" system-type)
;; => 应该显示 darwin
```

- 如果不是 macOS，请使用其他方式：
  - [GPG 加密文件](SECURITY_GPG.zh.md)
  - [环境变量](SECURITY_ENVIRONMENT.zh.md)

### 错误："API Key 未找到"

**原因 1：** Keychain 中没有注册该项目

**解决方法：**

使用以下命令确认 `api.openai.com` 的项目是否存在：

```bash
security find-internet-password -s api.openai.com
```

**原因 2：** 服务器名不匹配

**解决方法：**

确认 Keychain 中注册的服务器名与 `api.openai.com` 完全一致（包括大小写）。

**原因 3：** 账户名不匹配

**解决方法：**

确认账户名与 `apikey` 完全一致。

### Emacs 无法访问 Keychain

**原因：** Emacs 没有访问 Keychain 的权限

**解决方法：**

在 Emacs 启动时显示的对话框中选择 "始终允许"。

## Keychain 项目管理

### 更新项目

如需更改 API Key，请再次执行 `M-x sumibi-setup-api-key`。现有项目会自动更新：

```
M-x sumibi-setup-api-key
```

输入新的 API Key 后，会显示以下信息：

```
API Key 已保存到 Keychain (host: api.openai.com)
```

### 高级用户：通过命令行管理

如有需要，也可以使用 `security` 命令直接管理：

```bash
# 查看项目
security find-internet-password -s api.openai.com

# 删除项目
security delete-internet-password -s api.openai.com
```

## 安全注意事项

### 推荐事项

1. **选择 "始终允许"**
   - 避免每次都显示提示

2. **启用 FileVault**
   - 系统偏好设置 -> 安全性与隐私 -> FileVault
   - 加密整个磁盘以增强安全性

3. **使用强密码保护 Mac 登录**
   - Keychain 受 Mac 登录密码保护

4. **定期更换 API Key**
   - 每 3-6 个月生成新的 API Key 并更新 Keychain

### 注意事项

1. **不要与他人共享 Mac**
   - 能够登录的人可能访问 Keychain

2. **让 Mac 进入睡眠状态**
   - 离开时务必让 Mac 进入睡眠或锁定状态

3. **注意 Time Machine 备份**
   - 备份中也包含 Keychain，请注意备份磁盘的管理

## 与 iCloud Keychain 的关系

通过 `security add-internet-password` 命令添加的项目，默认保存在本地的登录 Keychain 中。

### 关于与 iCloud Keychain 的同步

如果在系统偏好设置中启用了 iCloud Keychain，**部分项目可能会自动同步到 iCloud**。

**同步的优点：**
- 可以在多台 Mac 之间共享 API Key
- 更换机器时自动迁移

**同步的缺点：**
- API Key 会保存在 iCloud 上
- 安全风险略有增加

**注意：** 通过 `security` 命令添加的互联网密码通常保存在本地的登录 Keychain 中，除非明确更改设置，否则不会与 iCloud 同步。但根据 macOS 版本和 iCloud 设置，行为可能有所不同。

## 参考资料

- [Apple - Keychain 访问](https://support.apple.com/zh-cn/guide/keychain-access/)
- [Apple - iCloud Keychain](https://support.apple.com/zh-cn/HT204085)
- [Emacs auth-source 文档](https://www.gnu.org/software/emacs/manual/html_node/auth/)

---

[<- 返回安全指南首页](SECURITY.zh.md)
