# セキュリティ

## API Keyの安全な管理

### 概要

Sumibi は OpenAI、Google Gemini、その他の LLM サービスの API を利用して日本語入力を行います。これらのサービスを利用するには API Key が必要ですが、API Key は **機密情報** であり、適切に管理する必要があります。

バージョン 5.0.0 以降、Sumibi は API Key を安全に保存するための3つの方法をサポートしています：

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

## 設定方法を選択

使用したい方法を選んで、詳細な設定手順をご覧ください：

### 📄 [方法1: 環境変数（デフォルト）](SECURITY_ENVIRONMENT.md)

従来の方法です。最も簡単ですが、セキュリティレベルは低くなります。

- ✅ 設定が簡単
- ✅ すべての OS で動作
- ❌ API Key が平文で保存される

**→ [環境変数の設定手順を見る](SECURITY_ENVIRONMENT.md)**

---

### 🔒 [方法2: GPG暗号化ファイル](SECURITY_GPG.md)

パスワードで保護された暗号化ファイルに API Key を保存します。

- ✅ API Key がパスワードで暗号化される
- ✅ dotfiles を公開しても安全
- ✅ すべての OS で動作

**→ [GPG暗号化の設定手順を見る](SECURITY_GPG.md)**

---

### 🔑 [方法3: macOS Keychain](SECURITY_KEYCHAIN.md)

macOS 標準のキーチェーンに API Key を保存します。

- ✅ macOS 標準の安全なストレージ
- ✅ パスワード入力が不要（または Touch ID で認証）
- ⚠️ macOS でのみ利用可能

**→ [macOS Keychain の設定手順を見る](SECURITY_KEYCHAIN.md)**

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
   - セキュアな方法を使用する

3. **API Key をメールやチャットで送信しない**
   - 暗号化された方法で共有

4. **公開リポジトリに API Key をコミットしない**
   - `.gitignore` で除外

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
