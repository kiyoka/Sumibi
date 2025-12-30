# GPG暗号化ファイルによる API Key 管理

[← セキュリティトップに戻る](SECURITY.md)

## 概要

GPG（GnuPG）を使用して API Key を暗号化ファイルに保存します。パスワードで保護されるため、高いセキュリティレベルを実現できます。dotfiles を公開している場合や、企業・組織で使用する場合に推奨します。

**セキュリティレベル:** ⭐⭐⭐（高）
**設定の簡単さ:** ⭐⭐（中）
**対応OS:** Linux、macOS、Windows（WSL）

## メリット・デメリット

### ✅ メリット

- **API Key がパスワードで暗号化される** - 第三者が暗号化ファイルを取得しても、パスワードがなければ読めない
- **dotfiles を公開しても安全** - 暗号化ファイル（`~/.authinfo.gpg`）は公開しても問題ない
- **すべての OS で動作** - GPG がインストールされていれば、どの OS でも使用可能
- **複数の API Key を一元管理できる** - OpenAI、Gemini など複数のサービスの API Key を1つのファイルで管理

### ⚠️ デメリット

- **GPG のセットアップが必要** - GPG 鍵の生成が必要
- **初回起動時にパスワード入力が必要** - ただし、一定時間キャッシュされる

## 必要なもの

- `gpg` コマンド（GnuPG）

## 設定手順

### 1. GPG がインストールされているか確認

```bash
gpg --version
```

バージョン情報が表示されれば、GPG がインストールされています。

### 2. GPG をインストール（必要な場合）

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

### 3. GPG 鍵を作成（まだ持っていない場合）

```bash
gpg --gen-key
```

以下の情報を入力します：

1. **名前（Real name）**: あなたの名前（例: Taro Yamada）
2. **メールアドレス（Email address）**: あなたのメールアドレス（例: taro@example.com）
3. **パスワード**: GPG 鍵のパスワード（**重要: 忘れないでください**）

鍵の生成が完了すると、以下のようなメッセージが表示されます：

```
pub   rsa3072 2024-01-01 [SC] [expires: 2026-01-01]
      ABCD1234EFGH5678IJKL9012MNOP3456QRST7890
uid                      Taro Yamada <taro@example.com>
sub   rsa3072 2024-01-01 [E] [expires: 2026-01-01]
```

もし、ターミナルの状態が悪く上記のメッセージが表示されなかったときは、以下のコマンドで生成が成功しているか確認できます。

```bash
gpg --list-keys --keyid-format LONG
[keyboxd]
---------
pub   rsa3072 2024-01-01 [SC] [expires: 2026-01-01]
      ABCD1234EFGH5678IJKL9012MNOP3456QRST7890
uid                      Taro Yamada <taro@example.com>
sub   rsa3072 2024-01-01 [E] [expires: 2026-01-01]
```

**注記**: 生成された鍵は `~/.gnupg/` ディレクトリに保存されます。

### 4. authinfo ファイルを作成

`~/.authinfo` ファイルを作成（または既存のファイルに追記）：

```bash
cat > ~/.authinfo << 'EOF'
machine api.openai.com login apikey password sk-your-api-key-here
EOF
```

**注意:** `sk-your-api-key-here` を実際の API Key に置き換えてください。

#### 複数の API サービスを使用する場合

```bash
cat > ~/.authinfo << 'EOF'
machine api.openai.com login apikey password sk-openai-key-here
machine generativelanguage.googleapis.com login apikey password your-gemini-key-here
EOF
```

### 5. ファイルを暗号化

```bash
gpg --encrypt --recipient your-email@example.com ~/.authinfo
```

**注意:** `your-email@example.com` は、GPG 鍵作成時に使用したメールアドレスに置き換えてください。

`~/.authinfo.gpg` ファイルが作成されます。

### 6. 元のファイルを削除

**重要:** 平文のファイルを削除します。

```bash
rm ~/.authinfo
```

**確認:** `~/.authinfo` が存在しないことを確認

```bash
ls -la ~/.authinfo
# ls: ~/.authinfo: No such file or directory
```

### 7. 暗号化ファイルの内容を確認（オプション）

正しく暗号化されたか確認します：

```bash
gpg --decrypt ~/.authinfo.gpg
```

パスワードを入力すると、内容が表示されます。

### 8. Emacs の設定

`~/.emacs.d/init.el` または設定ファイルに追加：

```elisp
(setq sumibi-api-key-source 'auth-source-gpg)
```

## 使用時の動作

Emacs を起動して Sumibi を使用すると、初回のみ GPG パスワードの入力を求められます：

```
Enter passphrase for GPG key:
```

パスワードを入力すると、一定時間（デフォルト: 10分）キャッシュされるため、その間は再入力不要です。

## トラブルシューティング

### エラー: "gpgコマンドが見つかりません"

**原因:** GPG がインストールされていない

**解決方法:**

GPG をインストールします（上記「2. GPG をインストール」を参照）。

### エラー: "Inappropriate ioctl for device"（macOS）

**エラーメッセージ例:**

```
Error while decrypting with "/opt/homebrew/bin/gpg":
gpg: encrypted with cv25519 key, ID XXXXXXXXXXXX, created 2025-12-30
      "Your Name <your@email.com>"
gpg: public key decryption failed: Inappropriate ioctl for device
gpg: decryption failed: Inappropriate ioctl for device
```

**原因:** Emacs から GPG を呼び出す際に、パスワードプロンプトを表示するための適切な入力デバイスが設定されていない

**解決方法（macOS）:**

1. **pinentry-mac をインストール:**

```bash
brew install pinentry-mac
```

2. **gpg-agent.conf を作成:**

```bash
cat > ~/.gnupg/gpg-agent.conf << 'EOF'
pinentry-program /opt/homebrew/bin/pinentry-mac
EOF
```

3. **gpg-agent を再起動:**

```bash
gpgconf --kill gpg-agent
```

4. **Emacs を再起動:**

Emacs を再起動して、新しい GPG 設定を反映させます。

これで、Emacs から `.authinfo.gpg` ファイルを復号化する際に、macOS の GUI ダイアログでパスワードを入力できるようになります。

### エラー: "API Keyが見つかりません"

**原因1:** `~/.authinfo.gpg` が存在しない

**解決方法:**

```bash
ls -la ~/.authinfo.gpg
```

ファイルが存在しない場合は、上記の設定手順を再度実施します。

**原因2:** `~/.authinfo.gpg` の内容が正しくない

**解決方法:**

内容を確認します：

```bash
gpg --decrypt ~/.authinfo.gpg
```

以下の形式になっているか確認：

```
machine api.openai.com login apikey password sk-...
```

### エラー: "decryption failed: No secret key"

**原因:** GPG 鍵が存在しない、または異なる鍵で暗号化されている

**解決方法:**

1. **GPG 鍵の一覧を確認:**

```bash
gpg --list-secret-keys
```

2. **鍵が存在しない場合は作成:**

```bash
gpg --gen-key
```

3. **正しい鍵で再暗号化:**

```bash
# 一旦復号化
gpg --decrypt ~/.authinfo.gpg > ~/.authinfo

# 再暗号化
gpg --encrypt --recipient your-email@example.com ~/.authinfo

# 平文ファイルを削除
rm ~/.authinfo
```

### GPG パスワードを毎回聞かれる

**原因:** GPG エージェントのキャッシュ時間が短い

**解決方法:** キャッシュ時間を延長

`~/.gnupg/gpg-agent.conf` を作成または編集：

```bash
cat > ~/.gnupg/gpg-agent.conf << 'EOF'
default-cache-ttl 3600
max-cache-ttl 7200
EOF
```

- `default-cache-ttl 3600`: デフォルトキャッシュ時間 3600秒（1時間）
- `max-cache-ttl 7200`: 最大キャッシュ時間 7200秒（2時間）

GPG エージェントを再起動：

```bash
gpgconf --kill gpg-agent
```

次回起動時から設定が反映されます。

### Emacs で GPG パスワードプロンプトが表示されない

**原因:** GPG エージェントの設定が正しくない（特に macOS）

**解決方法1:** Pinentry を GUI モードに設定

`~/.gnupg/gpg-agent.conf` に追加：

```bash
# macOS の場合
pinentry-program /usr/local/bin/pinentry-mac

# Linux の場合
pinentry-program /usr/bin/pinentry-gtk-2
```

GPG エージェントを再起動：

```bash
gpgconf --kill gpg-agent
```

**解決方法2:** ターミナルから Emacs を起動

```bash
emacs
```

### ファイルのパーミッションエラー

**原因:** `~/.authinfo.gpg` のパーミッションが適切でない

**解決方法:**

```bash
chmod 600 ~/.authinfo.gpg
```

## セキュリティ上の注意

### ✅ 推奨事項

1. **GPG パスワードは強固なものにする**
   - 12文字以上、英数字記号を組み合わせる

2. **GPG パスワードを忘れないようにする**
   - パスワードマネージャーに保存するか、安全な場所にメモ

3. **GPG 鍵のバックアップを取る**
   - 秘密鍵をバックアップ: `gpg --export-secret-keys your-email@example.com > private-key-backup.asc`
   - バックアップは安全な場所（USB メモリなど）に保管

4. **`~/.authinfo.gpg` のバックアップを取る**
   - 暗号化されているので、クラウドストレージにバックアップしても安全

### ⚠️ 注意事項

1. **平文の `~/.authinfo` は必ず削除する**
   - 暗号化後は平文ファイルを残さない

2. **GPG パスワードを他人に教えない**
   - パスワードが漏れると暗号化の意味がない

3. **共有マシンでは GPG キャッシュ時間を短くする**
   - `default-cache-ttl 600`（10分）など

## GPG 鍵の管理

### GPG 鍵の確認

```bash
# 公開鍵の確認
gpg --list-keys

# 秘密鍵の確認
gpg --list-secret-keys
```

### GPG 鍵のバックアップ

```bash
# 秘密鍵のエクスポート
gpg --export-secret-keys your-email@example.com > gpg-private-key.asc

# 公開鍵のエクスポート
gpg --export your-email@example.com > gpg-public-key.asc
```

**重要:** バックアップファイルは安全な場所に保管してください。

### GPG 鍵のリストア

```bash
# 秘密鍵のインポート
gpg --import gpg-private-key.asc

# 公開鍵のインポート
gpg --import gpg-public-key.asc
```

## 参照

- [GnuPG 公式サイト](https://gnupg.org/)
- [GnuPG ドキュメント](https://gnupg.org/documentation/)
- [Emacs auth-source ドキュメント](https://www.gnu.org/software/emacs/manual/html_node/auth/)

---

[← セキュリティトップに戻る](SECURITY.md)
