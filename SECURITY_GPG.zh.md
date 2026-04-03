# 通过 GPG 加密文件管理 API Key

[<- 返回安全指南首页](SECURITY.zh.md)

## 概述

使用 GPG（GnuPG）将 API Key 保存在加密文件中。由于受密码保护，可以实现较高的安全级别。适用于公开 dotfiles 或在企业/组织中使用的场景。

**安全级别：** 高
**配置难度：** 中等
**支持的操作系统：** Linux、macOS、Windows（WSL）

## 优缺点

### 优点

- **API Key 经过密码加密** - 即使第三方获取了加密文件，没有密码也无法读取
- **公开 dotfiles 也安全** - 加密文件（`~/.authinfo.gpg`）公开也没有问题
- **支持所有操作系统** - 只要安装了 GPG，任何操作系统都可以使用
- **可以集中管理多个 API Key** - 在一个文件中管理 OpenAI、Gemini 等多个服务的 API Key

### 缺点

- **需要安装和配置 GPG** - 需要生成 GPG 密钥
- **首次启动时需要输入密码** - 但会缓存一段时间

## 所需条件

- `gpg` 命令（GnuPG）

## 配置步骤

### 1. 确认 GPG 是否已安装

```bash
gpg --version
```

如果显示版本信息，说明 GPG 已安装。

### 2. 安装 GPG（如果需要）

#### macOS

```bash
brew install gnupg
```

#### Ubuntu/Debian

```bash
sudo apt-get install gnupg
```

#### Fedora/RHEL

```bash
sudo dnf install gnupg
```

#### Windows（WSL）

```bash
sudo apt-get install gnupg
```

### 3. 创建 GPG 密钥（如果还没有）

```bash
gpg --gen-key
```

需要输入以下信息：

1. **姓名（Real name）**：您的姓名（例：Zhang San）
2. **邮箱地址（Email address）**：您的邮箱（例：zhangsan@example.com）
3. **密码**：GPG 密钥的密码（**重要：请牢记此密码**）

密钥生成完成后，会显示类似以下的信息：

```
pub   rsa3072 2024-01-01 [SC] [expires: 2026-01-01]
      ABCD1234EFGH5678IJKL9012MNOP3456QRST7890
uid                      Zhang San <zhangsan@example.com>
sub   rsa3072 2024-01-01 [E] [expires: 2026-01-01]
```

如果终端显示异常没有看到上述信息，可以用以下命令确认是否生成成功：

```bash
gpg --list-keys --keyid-format LONG
```

**注意**：生成的密钥保存在 `~/.gnupg/` 目录中。

### 4. 创建 authinfo 文件

创建 `~/.authinfo` 文件（或在现有文件中追加）：

```bash
cat > ~/.authinfo << 'EOF'
machine api.openai.com login apikey password sk-your-api-key-here
EOF
```

**注意：** 请将 `sk-your-api-key-here` 替换为您的实际 API Key。

#### 使用多个 API 服务的情况

```bash
cat > ~/.authinfo << 'EOF'
machine api.openai.com login apikey password sk-openai-key-here
machine generativelanguage.googleapis.com login apikey password your-gemini-key-here
EOF
```

### 5. 加密文件

```bash
gpg --encrypt --recipient your-email@example.com ~/.authinfo
```

**注意：** 请将 `your-email@example.com` 替换为创建 GPG 密钥时使用的邮箱地址。

这将创建 `~/.authinfo.gpg` 文件。

### 6. 删除原始文件

**重要：** 删除明文文件。

```bash
rm ~/.authinfo
```

**确认：** 确保 `~/.authinfo` 已不存在

```bash
ls -la ~/.authinfo
# ls: ~/.authinfo: No such file or directory
```

### 7. 验证加密文件的内容（可选）

确认是否正确加密：

```bash
gpg --decrypt ~/.authinfo.gpg
```

输入密码后会显示文件内容。

### 8. Emacs 配置

在 `~/.emacs.d/init.el` 或配置文件中添加：

```elisp
(setq sumibi-api-key-source 'auth-source-gpg)
```

## 使用时的行为

启动 Emacs 使用 Sumibi 时，首次使用时会要求输入 GPG 密码：

```
Enter passphrase for GPG key:
```

输入密码后，会缓存一段时间（默认：10 分钟），在此期间无需再次输入。

## 故障排除

### 错误："找不到 gpg 命令"

**原因：** GPG 未安装

**解决方法：**

安装 GPG（参见上文"2. 安装 GPG"）。

### 错误："Inappropriate ioctl for device"（macOS）

**错误信息示例：**

```
Error while decrypting with "/opt/homebrew/bin/gpg":
gpg: public key decryption failed: Inappropriate ioctl for device
gpg: decryption failed: Inappropriate ioctl for device
```

**原因：** 从 Emacs 调用 GPG 时，没有配置适当的密码输入设备

**解决方法（macOS）：**

1. **安装 pinentry-mac：**

```bash
brew install pinentry-mac
```

2. **创建 gpg-agent.conf：**

```bash
cat > ~/.gnupg/gpg-agent.conf << 'EOF'
pinentry-program /opt/homebrew/bin/pinentry-mac
EOF
```

3. **重启 gpg-agent：**

```bash
gpgconf --kill gpg-agent
```

4. **重启 Emacs：**

重启 Emacs 以应用新的 GPG 配置。

### 错误："API Key 未找到"

**原因 1：** `~/.authinfo.gpg` 不存在

**解决方法：**

```bash
ls -la ~/.authinfo.gpg
```

如果文件不存在，请重新执行上述配置步骤。

**原因 2：** `~/.authinfo.gpg` 的内容格式不正确

**解决方法：**

检查内容：

```bash
gpg --decrypt ~/.authinfo.gpg
```

确认格式如下：

```
machine api.openai.com login apikey password sk-...
```

### 错误："decryption failed: No secret key"

**原因：** GPG 密钥不存在，或使用了不同的密钥加密

**解决方法：**

1. **查看 GPG 密钥列表：**

```bash
gpg --list-secret-keys
```

2. **如果密钥不存在则创建：**

```bash
gpg --gen-key
```

3. **使用正确的密钥重新加密：**

```bash
# 先解密
gpg --decrypt ~/.authinfo.gpg > ~/.authinfo

# 重新加密
gpg --encrypt --recipient your-email@example.com ~/.authinfo

# 删除明文文件
rm ~/.authinfo
```

### 每次都要输入 GPG 密码

**原因：** GPG agent 的缓存时间太短

**解决方法：** 延长缓存时间

创建或编辑 `~/.gnupg/gpg-agent.conf`：

```bash
cat > ~/.gnupg/gpg-agent.conf << 'EOF'
default-cache-ttl 3600
max-cache-ttl 7200
EOF
```

- `default-cache-ttl 3600`：默认缓存时间 3600 秒（1 小时）
- `max-cache-ttl 7200`：最大缓存时间 7200 秒（2 小时）

重启 GPG agent：

```bash
gpgconf --kill gpg-agent
```

下次启动时配置将生效。

### Emacs 中不显示 GPG 密码输入框

**原因：** GPG agent 配置不正确（特别是 macOS）

**解决方法 1：** 将 Pinentry 设置为 GUI 模式

在 `~/.gnupg/gpg-agent.conf` 中添加：

```bash
# macOS 的情况
pinentry-program /usr/local/bin/pinentry-mac

# Linux 的情况
pinentry-program /usr/bin/pinentry-gtk-2
```

重启 GPG agent：

```bash
gpgconf --kill gpg-agent
```

**解决方法 2：** 从终端启动 Emacs

```bash
emacs
```

### 文件权限错误

**原因：** `~/.authinfo.gpg` 的权限不正确

**解决方法：**

```bash
chmod 600 ~/.authinfo.gpg
```

## 安全注意事项

### 推荐事项

1. **使用强密码保护 GPG 密钥**
   - 12 个字符以上，混合使用字母、数字和符号

2. **不要忘记 GPG 密码**
   - 保存在密码管理器中，或记录在安全的地方

3. **备份 GPG 密钥**
   - 备份私钥：`gpg --export-secret-keys your-email@example.com > private-key-backup.asc`
   - 将备份保存在安全的地方（如 U 盘）

4. **备份 `~/.authinfo.gpg`**
   - 由于已加密，备份到云存储也是安全的

### 注意事项

1. **务必删除明文的 `~/.authinfo`**
   - 加密后不要保留明文文件

2. **不要将 GPG 密码告诉他人**
   - 密码泄露会使加密失去意义

3. **在共享机器上缩短 GPG 缓存时间**
   - 例如 `default-cache-ttl 600`（10 分钟）

## GPG 密钥管理

### 查看 GPG 密钥

```bash
# 查看公钥
gpg --list-keys

# 查看私钥
gpg --list-secret-keys
```

### 备份 GPG 密钥

```bash
# 导出私钥
gpg --export-secret-keys your-email@example.com > gpg-private-key.asc

# 导出公钥
gpg --export your-email@example.com > gpg-public-key.asc
```

**重要：** 请将备份文件保存在安全的地方。

### 恢复 GPG 密钥

```bash
# 导入私钥
gpg --import gpg-private-key.asc

# 导入公钥
gpg --import gpg-public-key.asc
```

## 参考资料

- [GnuPG 官方网站](https://gnupg.org/)
- [GnuPG 文档](https://gnupg.org/documentation/)
- [Emacs auth-source 文档](https://www.gnu.org/software/emacs/manual/html_node/auth/)

---

[<- 返回安全指南首页](SECURITY.zh.md)
