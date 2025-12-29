# macOS Keychain による API Key 管理

[← セキュリティトップに戻る](SECURITY.md)

## 概要

macOS 標準のキーチェーンに API Key を保存します。macOS でのみ利用可能ですが、最も統合されたセキュアな方法です。Touch ID での認証や、パスワード入力の自動化など、macOS の機能をフル活用できます。

**セキュリティレベル:** ⭐⭐⭐（高）
**設定の簡単さ:** ⭐⭐⭐（簡単）
**対応OS:** macOS のみ

## メリット・デメリット

### ✅ メリット

- **macOS 標準の安全なストレージ** - Apple が提供する信頼性の高いセキュリティ機構
- **パスワード入力が不要** - またはTouch ID で認証（Mac の設定による）
- **設定が比較的簡単** - コマンドラインで簡単に設定可能
- **他のアプリケーションとキーチェーンを共有できる** - macOS の他のアプリケーションとも統合

### ⚠️ デメリット

- **macOS でのみ利用可能** - Linux や Windows では使用できない
- **マシンを変更した場合、再設定が必要** - キーチェーンは基本的にマシン固有

## 必要なもの

- macOS

## 設定手順

ターミナルから `security` コマンドを使用して設定します。この方法はすべてのmacOSバージョンで動作します。

#### 1. キーチェーンに追加

```bash
security add-internet-password \
  -a apikey \
  -s api.openai.com \
  -w "sk-your-api-key-here" \
  -U
```

**注意:** `sk-your-api-key-here` を実際の API Key に置き換えてください。

パスワードの入力を求められた場合は、Mac のログインパスワードを入力します。

#### 2. 登録を確認

```bash
security find-internet-password -s api.openai.com
```

項目が表示されれば、正常に登録されています。

#### 3. Emacs の設定

`~/.emacs.d/init.el` に以下を追加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

## 使用時の動作

Emacs を起動して Sumibi を使用すると、macOS がキーチェーンから API Key を自動的に取得します。

Emacs がキーチェーンの項目に初めてアクセスする時、以下のダイアログが表示されます：

```
"Emacs" が機密情報にアクセスしようとしています。
キーチェーン "login" のパスワードを入力してください。
```

- **「許可」** - 今回のみ許可
- **「常に許可」** - 今後は常に許可（推奨）

**注意:**
- 「**常に許可**」を選択すると、その後（macOS再起動後も含めて）ダイアログは表示されなくなります
- macOSにログイン後、ログインキーチェーンは自動的にアンロックされているため、macOS再起動直後に特別な操作は不要です
- 「許可」のみを選択した場合は、Emacsを起動するたびにダイアログが表示されます

## 複数の API サービスを使用する場合

OpenAI と Gemini など、複数のサービスを使用する場合は、サーバ名を変えて複数の項目を登録します。

### OpenAI の登録

```bash
security add-internet-password \
  -a apikey \
  -s api.openai.com \
  -w "sk-openai-key-here" \
  -U
```

### Google Gemini の登録

Gemini APIのエンドポイント `https://generativelanguage.googleapis.com/v1beta/openai/` を使用する場合、ホスト名部分のみを指定します：

```bash
security add-internet-password \
  -a apikey \
  -s generativelanguage.googleapis.com \
  -w "your-gemini-key-here" \
  -U
```

**注意:**
- `-s` パラメータにはホスト名のみを指定します（パスやプロトコルは含めません）
- 完全なエンドポイントURL: `https://generativelanguage.googleapis.com/v1beta/openai/`
- Keychain登録時のサーバー名: `generativelanguage.googleapis.com`

## トラブルシューティング

### エラー: "macOS KeychainはmacOSでのみ利用可能です"

**原因:** macOS 以外の OS で `auth-source-keychain` を使用しようとしている

**解決方法:**

- macOS を使用している場合は、Emacs が正しく OS を認識しているか確認:

```elisp
;; Emacs で以下を評価
(message "%s" system-type)
;; => darwin が表示されるはず
```

- macOS 以外の場合は、他の方法を使用:
  - [GPG暗号化ファイル](SECURITY_GPG.md)
  - [環境変数](SECURITY_ENVIRONMENT.md)

### エラー: "API Keyが見つかりません"

**原因1:** Keychain に項目が登録されていない

**解決方法:**

以下のコマンドで `api.openai.com` の項目が存在するか確認します：

```bash
security find-internet-password -s api.openai.com
```

**原因2:** サーバ名が一致しない

**解決方法:**

キーチェーンに登録したサーバ名が `api.openai.com` と完全に一致しているか確認します（大文字小文字も含む）。

**原因3:** アカウント名が一致しない

**解決方法:**

アカウント名が `apikey` と完全に一致しているか確認します。

### Emacs がキーチェーンにアクセスできない

**原因:** Emacs にキーチェーンへのアクセス権限がない

**解決方法:**

Emacs 起動時に表示されるダイアログで「常に許可」を選択します。

## キーチェーン項目の管理

### 項目の確認

```bash
security find-internet-password -s api.openai.com
```

### 項目の削除

```bash
security delete-internet-password -s api.openai.com
```

### 項目の更新

API Key を変更する場合：

```bash
# 古い項目を削除
security delete-internet-password -s api.openai.com

# 新しい項目を追加
security add-internet-password \
  -a apikey \
  -s api.openai.com \
  -w "sk-new-api-key-here" \
  -U
```

## セキュリティ上の注意

### ✅ 推奨事項

1. **「常に許可」を選択する**
   - 毎回プロンプトが表示されるのを防ぐ

2. **FileVault を有効にする**
   - システム環境設定 → セキュリティとプライバシー → FileVault
   - ディスク全体を暗号化してセキュリティを強化

3. **Mac のログインパスワードを強固にする**
   - キーチェーンは Mac のログインパスワードで保護される

4. **定期的に API Key をローテーションする**
   - 3〜6ヶ月ごとに新しい API Key を発行し、キーチェーンを更新

### ⚠️ 注意事項

1. **Mac を他人と共有しない**
   - ログインできる人はキーチェーンにアクセスできる可能性がある

2. **Mac をスリープさせる**
   - 席を離れる際は必ずスリープまたはロックする

3. **Time Machine バックアップに注意**
   - バックアップにもキーチェーンが含まれるため、バックアップディスクの管理に注意

## iCloud キーチェーンとの関係

`security add-internet-password` コマンドで追加した項目は、デフォルトでローカルのログインキーチェーンに保存されます。

### iCloud キーチェーンとの同期について

システム環境設定でiCloudキーチェーンを有効にしている場合、**一部の項目が自動的にiCloudと同期される可能性があります**。

**同期される場合のメリット:**
- 複数の Mac で API Key を共有できる
- 機種変更時に自動的に移行される

**同期される場合のデメリット:**
- iCloud に API Key が保存される
- セキュリティリスクがわずかに増加

**注意:** `security` コマンドで追加したインターネットパスワードは、通常はローカルのログインキーチェーンに保存され、明示的な設定変更がない限りiCloudと同期されません。ただし、macOSのバージョンやiCloud設定により動作が異なる場合があります。

## 参照

- [Apple - キーチェーンアクセス](https://support.apple.com/ja-jp/guide/keychain-access/)
- [Apple - iCloud キーチェーン](https://support.apple.com/ja-jp/HT204085)
- [Emacs auth-source ドキュメント](https://www.gnu.org/software/emacs/manual/html_node/auth/)

---

[← セキュリティトップに戻る](SECURITY.md)

