# セキュリティ

## API Keyの安全な管理

### 概要

Sumibi は OpenAI、Google Gemini、その他の LLM サービスの API を利用して日本語入力を行います。これらのサービスを利用するには API Key が必要ですが、API Key は **機密情報** であり、適切に管理する必要があります。

バージョン 3.8.0 以降、Sumibi は API Key を安全に保存するための3つの方法をサポートしています：

1. **環境変数**（デフォルト）- 従来の方法
2. **GPG暗号化ファイル** - パスワードで保護された暗号化ファイル
3. **macOS Keychain** - macOS の安全なキーチェーン

### なぜAPI Keyの安全な管理が重要か

API Key が漏洩すると、以下のようなリスクがあります：

- 🚨 **不正利用による課金** - 第三者があなたの API Key を使用して高額な API 呼び出しを行う可能性
- 🚨 **アカウント停止** - 利用規約違反として API アクセスが停止される可能性
- 🚨 **データ漏洩** - あなたの入力履歴が第三者に読み取られる可能性

特に、以下の場合は **環境変数以外の方法** を強く推奨します：

- ✅ dotfiles を GitHub などに公開している
- ✅ 複数人で同じマシンを使用している
- ✅ より高いセキュリティレベルが必要

## 3つの管理方法の比較

| 方法 | セキュリティ | 設定の簡単さ | 対応OS | おすすめ度 |
|------|------------|------------|--------|----------|
| 環境変数 | ⭐ | ⭐⭐⭐ | すべて | 個人利用 |
| GPG暗号化 | ⭐⭐⭐ | ⭐⭐ | Linux/macOS/Windows | 推奨 |
| macOS Keychain | ⭐⭐⭐ | ⭐⭐⭐ | macOS のみ | macOS で推奨 |

## 設定方法

### 方法1: 環境変数（デフォルト）

従来の方法です。最も簡単ですが、セキュリティレベルは低くなります。

#### 設定手順

**1. 環境変数を設定**

`~/.bashrc`、`~/.zshrc`、または `~/.profile` に以下を追加：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
# または
export OPENAI_API_KEY="sk-your-api-key-here"
```

**2. Emacs の設定**

特に設定は不要です（デフォルト動作）。

または、明示的に指定する場合：

```elisp
(setq sumibi-api-key-source 'environment)
```

#### メリット・デメリット

**メリット:**
- ✅ 設定が簡単
- ✅ すべての OS で動作

**デメリット:**
- ❌ API Key が平文で保存される
- ❌ dotfiles を公開すると API Key も公開される
- ❌ プロセス一覧から API Key が見える可能性

---

### 方法2: GPG暗号化ファイル

GPG を使用して API Key を暗号化ファイルに保存します。パスワードで保護されるため、高いセキュリティレベルを実現できます。

#### 必要なもの

- `gpg` コマンド（GnuPG）

#### 設定手順

**1. GPG がインストールされているか確認**

```bash
gpg --version
```

インストールされていない場合：

```bash
# macOS
brew install gnupg

# Ubuntu/Debian
sudo apt-get install gnupg

# Fedora/RHEL
sudo dnf install gnupg
```

**2. GPG 鍵を作成（まだ持っていない場合）**

```bash
gpg --gen-key
```

指示に従って、名前、メールアドレス、パスワードを設定します。

**3. authinfo ファイルを作成**

`~/.authinfo` ファイルを作成（または既存のファイルに追記）：

```
machine api.openai.com login apikey password sk-your-api-key-here
```

**注意:** 実際の API Key に置き換えてください。

**4. ファイルを暗号化**

```bash
gpg --encrypt --recipient your-email@example.com ~/.authinfo
```

`~/.authinfo.gpg` ファイルが作成されます。

**5. 元のファイルを削除**

```bash
rm ~/.authinfo
```

**6. Emacs の設定**

`~/.emacs.d/init.el` または設定ファイルに追加：

```elisp
(setq sumibi-api-key-source 'auth-source-gpg)
```

#### 使用時の動作

初回起動時に GPG パスワードの入力を求められます。パスワードは一定時間キャッシュされるため、毎回入力する必要はありません。

#### メリット・デメリット

**メリット:**
- ✅ API Key がパスワードで暗号化される
- ✅ dotfiles を公開しても安全
- ✅ すべての OS で動作（GPG がインストールされていれば）
- ✅ 複数の API Key を一元管理できる

**デメリット:**
- ⚠️ GPG のセットアップが必要
- ⚠️ 初回起動時にパスワード入力が必要

---

### 方法3: macOS Keychain

macOS 標準のキーチェーンに API Key を保存します。macOS でのみ利用可能ですが、最も統合されたセキュアな方法です。

#### 必要なもの

- macOS

#### 設定手順

**1. キーチェーンアクセスを開く**

アプリケーション → ユーティリティ → キーチェーンアクセス

**2. 新しいパスワード項目を作成**

- メニューから「ファイル」→「新規パスワード項目」を選択
- または、`⌘N` を押す

**3. 項目の情報を入力**

- **キーチェーン項目名**: `api.openai.com`（または任意の名前）
- **アカウント名**: `apikey`
- **パスワード**: 実際の API Key（例: `sk-...`）

**4. 保存**

「追加」ボタンをクリック

**注意:** より確実に動作させるには、「インターネットパスワード」として登録することをおすすめします：

1. キーチェーンアクセスで右クリック → 「検索」
2. 新規項目を作成
3. 種類: **インターネットパスワード**
4. サーバ: `api.openai.com`
5. アカウント: `apikey`
6. パスワード: 実際の API Key

**5. Emacs の設定**

`~/.emacs.d/init.el` または設定ファイルに追加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

#### メリット・デメリット

**メリット:**
- ✅ macOS 標準の安全なストレージ
- ✅ パスワード入力が不要（または Touch ID で認証）
- ✅ 設定が比較的簡単
- ✅ 他のアプリケーションとキーチェーンを共有できる

**デメリット:**
- ⚠️ macOS でのみ利用可能
- ⚠️ マシンを変更した場合、再設定が必要

---

## トラブルシューティング

### エラー: "gpgコマンドが見つかりません"

**原因:** GPG がインストールされていない

**解決方法:**

```bash
# macOS
brew install gnupg

# Ubuntu/Debian
sudo apt-get install gnupg
```

### エラー: "macOS KeychainはmacOSでのみ利用可能です"

**原因:** macOS 以外の OS で `auth-source-keychain` を使用しようとしている

**解決方法:**

- macOS を使用している場合は、Emacs が正しく OS を認識しているか確認
- macOS 以外の場合は、`auth-source-gpg` または `environment` を使用

### エラー: "API Keyが見つかりません"

**原因1:** authinfo ファイルまたは Keychain に正しく登録されていない

**解決方法:**
- `~/.authinfo.gpg` の内容を確認（`gpg --decrypt ~/.authinfo.gpg`）
- Keychain で `api.openai.com` の項目が存在するか確認

**原因2:** 環境変数が設定されていない

**解決方法:**
```bash
echo $SUMIBI_AI_API_KEY
# または
echo $OPENAI_API_KEY
```

### GPG パスワードを毎回聞かれる

**解決方法:** GPG エージェントのキャッシュ時間を延長

`~/.gnupg/gpg-agent.conf` を編集：

```
default-cache-ttl 3600
max-cache-ttl 7200
```

GPG エージェントを再起動：

```bash
gpgconf --kill gpg-agent
```

---

## セキュリティのベストプラクティス

### ✅ 推奨事項

1. **GPG または Keychain を使用する**
   - 環境変数よりも安全

2. **API Key を定期的にローテーションする**
   - 3〜6ヶ月ごとに新しい API Key を発行

3. **不要になった API Key は無効化する**
   - OpenAI や Gemini の管理画面で古い Key を削除

4. **dotfiles を公開する場合は環境変数を使用しない**
   - GPG または Keychain を使用

5. **API Key の使用量を監視する**
   - 不正利用の早期発見のため

### ❌ 避けるべきこと

1. **API Key をソースコードに埋め込まない**
   - Git にコミットすると履歴に残る

2. **API Key をプレーンテキストで保存しない**
   - `~/.bashrc` に直接書くのは避ける

3. **API Key をメールやチャットで送信しない**
   - 暗号化された方法で共有

4. **公開リポジトリに API Key をコミットしない**
   - `.gitignore` で除外

---

## 複数の API サービスを使用する場合

### Gemini など他の API を使用する場合

`~/.authinfo.gpg` に複数のエントリを追加できます：

```
machine api.openai.com login apikey password sk-openai-key-here
machine generativelanguage.googleapis.com login apikey password your-gemini-key-here
```

macOS Keychain の場合も、サーバ名を変えて複数の項目を登録できます。

---

## 参照

- [GnuPG 公式サイト](https://gnupg.org/)
- [Emacs auth-source ドキュメント](https://www.gnu.org/software/emacs/manual/html_node/auth/)
- [OpenAI API Keys 管理](https://platform.openai.com/api-keys)
- [Google AI Studio](https://makersuite.google.com/app/apikey)

---

## 脆弱性の報告

Sumibi のセキュリティ上の問題を発見した場合は、GitHub Issues ではなく、メンテナーに直接報告してください：

- リポジトリ: https://github.com/kiyoka/Sumibi
- セキュリティポリシー: GitHub Security Advisory を使用
