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

### ⚠️ macOS 15（Sequoia）以降をお使いの方へ

macOS 15では新しい「パスワード」アプリが追加されましたが、**Sumibiでは従来の「キーチェーンアクセス」アプリを使用します**。

- **キーチェーンアクセスはまだ利用可能です** - Spotlightで「Keychain Access」または「キーチェーンアクセス」と検索すれば開けます
- **「パスワード」アプリは使用できません** - auth-sourceがインターネットパスワード形式を必要とするため
- **最も確実な方法**: 以下の「方法B: コマンドラインでの設定（推奨）」を使用してください

---

### 方法A: GUI での設定

⚠️ **macOS 15以降の場合は、方法B（コマンドライン）を推奨します。**

#### 1. キーチェーンアクセスを開く

- **Spotlight で検索**: `⌘ + Space` を押して「keychain」または「Keychain Access」と入力

#### 2. インターネットパスワードとして登録

キーチェーンアクセスで以下の操作を行います：

1. メニューから「ファイル」→「新規パスワード項目」を選択、または `⌘N` を押す
2. 以下の情報を入力：

   - **キーチェーン項目名**: `api.openai.com`
   - **アカウント名**: `apikey`
   - **パスワード**: 実際の API Key（例: `sk-proj-...`）

3. 「追加」ボタンをクリック

**注意**: macOS 15以降では、「新規インターネットパスワード」メニューが利用できない場合があります。その場合は方法B（コマンドライン）を使用してください。

#### 3. 登録を確認

キーチェーンアクセスの検索バーで `api.openai.com` を検索し、項目が表示されることを確認します。

#### 3-1. 複数のAPIサービスを使用する場合（オプション）

Google Gemini APIも使用する場合は、追加で登録します：

1. メニューから「ファイル」→「新規パスワード項目」を選択、または `⌘N` を押す
2. 以下の情報を入力：

   - **キーチェーン項目名**: `generativelanguage.googleapis.com`
   - **アカウント名**: `apikey`
   - **パスワード**: 実際の Gemini API Key

3. 「追加」ボタンをクリック

**注意:** Gemini APIのエンドポイント `https://generativelanguage.googleapis.com/v1beta/openai/` を使用する場合、ホスト名部分 `generativelanguage.googleapis.com` のみを項目名として登録します。

#### 4. Emacs の設定

`~/.emacs.d/init.el` または設定ファイルに追加：

```elisp
(setq sumibi-api-key-source 'auth-source-keychain)
(setenv "SUMIBI_AI_BASEURL" "https://generativelanguage.googleapis.com/v1beta/openai/")
(setenv "SUMIBI_AI_MODEL" "gemini-2.5-flash")
```

SUMIBI_AI_MODELには好きなモデル名を指定してください。

#### 5. Emacs を再起動

設定を反映するために Emacs を再起動します。

---

### 方法B: コマンドラインでの設定（macOS 15以降で推奨）

ターミナルから `security` コマンドを使用して設定します。この方法はすべてのmacOSバージョンで動作し、特にmacOS 15以降で最も確実です。

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

## macOS 15以降の「パスワード」アプリについて

macOS 15（Sequoia）以降では、「パスワード」という新しいアプリが追加されました。

### 「パスワード」アプリと「キーチェーンアクセス」の違い

- **パスワードアプリ**: ウェブサイトやアプリのパスワード管理に特化した新しいGUI
- **キーチェーンアクセス**: 証明書、認証トークン、セキュアノート、インターネットパスワードなど、より広範な機密情報を管理

### Sumibiで「パスワード」アプリを使用できますか？

**現時点では推奨しません。** 理由：

1. **auth-sourceの互換性**: auth-sourceライブラリは従来のインターネットパスワード形式を期待します
2. **確実性**: `security` コマンドでインターネットパスワードとして登録する方が確実です

### 「パスワード」アプリでAPI Keyを保存したい場合

技術的には可能ですが、動作は未検証です：

1. 「パスワード」アプリを開く
2. 「+」アイコンをクリック
3. 「Webサイト、アプリまたはラベル」に `api.openai.com` を入力
4. ユーザ名: `apikey`
5. パスワード: 実際のAPI Key
6. 保存

**注意**: この方法でauth-sourceが正しく認識するかは保証されません。問題が発生した場合は、方法B（コマンドライン）を使用してください。

## 参照

- [Apple - キーチェーンアクセス](https://support.apple.com/ja-jp/guide/keychain-access/)
- [Apple - iCloud キーチェーン](https://support.apple.com/ja-jp/HT204085)
- [Apple - パスワードアプリの使い方](https://support.apple.com/en-us/120758)
- [Emacs auth-source ドキュメント](https://www.gnu.org/software/emacs/manual/html_node/auth/)
- [macOS 15 Sequoia のパスワード管理について（英語）](https://www.macobserver.com/tips/how-to/manage-your-passwords-with-the-apple-passwords-app/)

---

[← セキュリティトップに戻る](SECURITY.md)

