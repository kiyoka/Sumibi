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
- **設定が比較的簡単** - GUI で直感的に設定可能
- **他のアプリケーションとキーチェーンを共有できる** - macOS の他のアプリケーションとも統合

### ⚠️ デメリット

- **macOS でのみ利用可能** - Linux や Windows では使用できない
- **マシンを変更した場合、再設定が必要** - キーチェーンは基本的にマシン固有

## 必要なもの

- macOS

## 設定手順

### 方法A: GUI での設定（推奨）

#### 1. キーチェーンアクセスを開く

以下のいずれかの方法で開きます：

- **Spotlight で検索**: `⌘ + Space` を押して「キーチェーンアクセス」と入力
- **Finder から**: アプリケーション → ユーティリティ → キーチェーンアクセス
- **Launchpad から**: ユーティリティフォルダ内のキーチェーンアクセス

#### 2. 新しいパスワード項目を作成

**オプション1: 簡易的な方法**

1. メニューから「ファイル」→「新規パスワード項目」を選択
2. または、`⌘N` を押す

以下の情報を入力：

- **キーチェーン項目名**: `api.openai.com`
- **アカウント名**: `apikey`
- **パスワード**: 実際の API Key（例: `sk-proj-...`）

3. 「追加」ボタンをクリック

**オプション2: より確実な方法（推奨）**

インターネットパスワードとして登録します：

1. キーチェーンアクセスのメイン画面で「+」ボタンをクリック、または右クリック → 「新規インターネットパスワード」
2. 以下の情報を入力：

   - **種類**: インターネットパスワード
   - **サーバ**: `api.openai.com`
   - **アカウント名**: `apikey`
   - **パスワード**: 実際の API Key（例: `sk-proj-...`）
   - **プロトコル**: HTTPS
   - **セキュリティドメイン**: （空欄）

3. 「追加」をクリック

#### 3. 登録を確認

キーチェーンアクセスの検索バーで `api.openai.com` を検索し、項目が表示されることを確認します。

#### 4. Emacs の設定

`~/.emacs.d/init.el` または設定ファイルに追加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

#### 5. Emacs を再起動

設定を反映するために Emacs を再起動します。

---

### 方法B: コマンドラインでの設定

ターミナルから `security` コマンドを使用して設定することもできます。

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

方法A と同じく、`~/.emacs.d/init.el` に以下を追加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
```

## 使用時の動作

Emacs を起動して Sumibi を使用すると、macOS がキーチェーンから API Key を自動的に取得します。

初回のみ、以下のダイアログが表示される場合があります：

```
"Emacs" が機密情報にアクセスしようとしています。
キーチェーン "login" のパスワードを入力してください。
```

- **「許可」** - 今回のみ許可
- **「常に許可」** - 今後は常に許可（推奨）

「常に許可」を選択すると、次回からは自動的にアクセスできます。

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

### Gemini の登録（例）

```bash
security add-internet-password \
  -a apikey \
  -s generativelanguage.googleapis.com \
  -w "your-gemini-key-here" \
  -U
```

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

キーチェーンアクセスを開いて、`api.openai.com` の項目が存在するか確認します。

**原因2:** サーバ名が一致しない

**解決方法:**

キーチェーンに登録したサーバ名が `api.openai.com` と完全に一致しているか確認します（大文字小文字も含む）。

**原因3:** アカウント名が一致しない

**解決方法:**

アカウント名が `apikey` と完全に一致しているか確認します。

### Emacs がキーチェーンにアクセスできない

**原因:** Emacs にキーチェーンへのアクセス権限がない

**解決方法1:** 「常に許可」を選択

Emacs 起動時に表示されるダイアログで「常に許可」を選択します。

**解決方法2:** キーチェーンの設定を確認

1. キーチェーンアクセスを開く
2. `api.openai.com` の項目をダブルクリック
3. 「アクセス制御」タブを選択
4. 「すべてのアプリケーションにこの項目へのアクセスを許可」をチェック
5. または、「常にアクセスを許可するアプリケーション」に Emacs を追加

### GUI から Emacs を起動するとキーチェーンにアクセスできない

**原因:** GUI アプリケーションとして起動した Emacs が、正しくキーチェーンにアクセスできない場合がある

**解決方法1:** ターミナルから Emacs を起動

```bash
/Applications/Emacs.app/Contents/MacOS/Emacs
```

または

```bash
open -a Emacs
```

**解決方法2:** Emacs.app を「常に許可」に追加

キーチェーンアクセスで項目を開き、アクセス制御タブで Emacs.app を追加します。

## キーチェーン項目の管理

### 項目の確認

```bash
security find-internet-password -s api.openai.com
```

### 項目の削除

```bash
security delete-internet-password -s api.openai.com
```

GUI の場合は、キーチェーンアクセスで項目を選択して Delete キーを押します。

### 項目の更新

API Key を変更する場合：

1. **GUI の場合:**
   - キーチェーンアクセスで項目をダブルクリック
   - 「パスワードを表示」をチェック
   - Mac のパスワードを入力
   - 新しい API Key を入力
   - 保存

2. **コマンドラインの場合:**

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

macOS のキーチェーンは、iCloud キーチェーンと同期される場合があります。

### iCloud キーチェーンを使用する場合

**メリット:**
- 複数の Mac で API Key を共有できる
- iPhone/iPad でもアクセス可能（ただし Sumibi は macOS のみ）

**デメリット:**
- iCloud に API Key が保存される
- セキュリティリスクがわずかに増加

### iCloud キーチェーンを無効にする場合

特定の項目を iCloud と同期しないようにするには：

1. キーチェーンアクセスで項目を選択
2. 「キーチェーン」列で「ログイン」を選択（「iCloud」ではなく）

## 参照

- [Apple - キーチェーンアクセス](https://support.apple.com/ja-jp/guide/keychain-access/)
- [Apple - iCloud キーチェーン](https://support.apple.com/ja-jp/HT204085)
- [Emacs auth-source ドキュメント](https://www.gnu.org/software/emacs/manual/html_node/auth/)

---

[← セキュリティトップに戻る](SECURITY.md)
