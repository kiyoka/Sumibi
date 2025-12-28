# 環境変数による API Key 管理

[← セキュリティトップに戻る](SECURITY.md)

## 概要

環境変数を使用した API Key の管理は、最も簡単な方法です。設定ファイル（`~/.bashrc` や `~/.zshrc`）に環境変数を追加するだけで利用できます。

**セキュリティレベル:** ⭐（低）
**設定の簡単さ:** ⭐⭐⭐（簡単）
**対応OS:** すべて

## メリット・デメリット

### ✅ メリット

- **設定が簡単** - シェルの設定ファイルに1行追加するだけ
- **すべての OS で動作** - Linux、macOS、Windows（WSL）で利用可能
- **追加のツール不要** - GPG や Keychain のセットアップ不要

### ❌ デメリット

- **API Key が平文で保存される** - 暗号化されずにファイルに記録される
- **dotfiles を公開すると API Key も公開される** - GitHub などに dotfiles を公開している場合は危険
- **プロセス一覧から API Key が見える可能性** - `ps` コマンドなどで環境変数が見える場合がある

## 設定手順

### 1. 環境変数を設定

使用しているシェルの設定ファイルに環境変数を追加します。

#### bash を使用している場合

`~/.bashrc` または `~/.bash_profile` を編集：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
# または
export OPENAI_API_KEY="sk-your-api-key-here"
```

#### zsh を使用している場合（macOS デフォルト）

`~/.zshrc` を編集：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
# または
export OPENAI_API_KEY="sk-your-api-key-here"
```

#### fish を使用している場合

`~/.config/fish/config.fish` を編集：

```fish
set -x SUMIBI_AI_API_KEY "sk-your-api-key-here"
# または
set -x OPENAI_API_KEY "sk-your-api-key-here"
```

**注意:** 実際の API Key に置き換えてください。

### 2. 設定を反映

設定ファイルを編集したら、以下のコマンドで設定を反映します：

```bash
# bash の場合
source ~/.bashrc

# zsh の場合
source ~/.zshrc

# fish の場合
source ~/.config/fish/config.fish
```

または、ターミナルを再起動します。

### 3. 環境変数が設定されたか確認

```bash
echo $SUMIBI_AI_API_KEY
# または
echo $OPENAI_API_KEY
```

API Key が表示されれば設定完了です。

### 4. Emacs の設定（オプション）

特に設定は不要です（デフォルト動作）。

明示的に指定する場合は、`~/.emacs.d/init.el` に以下を追加：

```elisp
(setq sumibi-api-key-source 'environment)
```

## 複数の API サービスを使用する場合

OpenAI と Gemini など、複数のサービスを使い分ける場合は、サービスごとに環境変数を設定できます：

```bash
# OpenAI
export OPENAI_API_KEY="sk-openai-key-here"

# Gemini（例）
export GEMINI_API_KEY="your-gemini-key-here"
```

Sumibi は現在 `SUMIBI_AI_API_KEY` または `OPENAI_API_KEY` のみをサポートしています。

## トラブルシューティング

### エラー: "API Keyが見つかりません"

**原因:** 環境変数が設定されていない

**解決方法:**

1. **環境変数が設定されているか確認:**

```bash
echo $SUMIBI_AI_API_KEY
echo $OPENAI_API_KEY
```

何も表示されない場合は、環境変数が設定されていません。

2. **設定ファイルを確認:**

設定ファイル（`~/.bashrc` または `~/.zshrc`）を開いて、`export` 行があるか確認します。

3. **設定を反映:**

```bash
source ~/.bashrc  # または source ~/.zshrc
```

4. **Emacs を再起動:**

環境変数の変更後は、Emacs を再起動する必要があります。

### Emacs で環境変数が読み込まれない

**原因:** Emacs が GUI アプリとして起動した場合、シェルの環境変数を継承しない場合がある（特に macOS）

**解決方法1:** ターミナルから Emacs を起動

```bash
emacs
```

**解決方法2:** `exec-path-from-shell` パッケージを使用

`~/.emacs.d/init.el` に追加：

```elisp
(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "SUMIBI_AI_API_KEY")
    (exec-path-from-shell-copy-env "OPENAI_API_KEY")))
```

### dotfiles を公開している場合の注意

**問題:** dotfiles を GitHub などに公開すると、API Key も公開されてしまう

**解決方法1:** 別ファイルに分離

`~/.bashrc` または `~/.zshrc` に以下を追加：

```bash
# 秘密情報を別ファイルに分離
if [ -f ~/.secrets ]; then
    source ~/.secrets
fi
```

`~/.secrets` ファイルを作成（`.gitignore` に追加）：

```bash
export SUMIBI_AI_API_KEY="sk-your-api-key-here"
```

`.gitignore` に追加：

```
.secrets
```

**解決方法2:** GPG または Keychain を使用

環境変数ではなく、より安全な方法を使用します：

- [GPG暗号化ファイルを使用する](SECURITY_GPG.md)
- [macOS Keychain を使用する](SECURITY_KEYCHAIN.md)（macOS のみ）

## セキュリティ上の注意

### ⚠️ 重要な注意事項

1. **dotfiles を公開する場合は、API Key を別ファイルに分離する**
   - GitHub などに公開する場合は特に注意

2. **共有マシンでは使用しない**
   - 複数人で使うマシンでは、他のユーザーが環境変数を見る可能性がある

3. **定期的に API Key をローテーションする**
   - 3〜6ヶ月ごとに新しい API Key を発行

4. **古い API Key は無効化する**
   - OpenAI や Gemini の管理画面で削除

## より安全な方法への移行

環境変数よりも安全な方法への移行を検討してください：

- **[GPG暗号化ファイル](SECURITY_GPG.md)** - パスワードで保護された暗号化ファイル（推奨）
- **[macOS Keychain](SECURITY_KEYCHAIN.md)** - macOS 標準のセキュアストレージ（macOS のみ）

---

[← セキュリティトップに戻る](SECURITY.md)
