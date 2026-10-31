# TESTING.md

このドキュメントは、Sumibiプロジェクトのユニットテストの実行方法とガイドラインを説明します。

## テストの実行方法

### 基本的なテスト実行
```bash
make test
```

### 個別テスト実行
```bash
# 特定のテストのみ実行
emacs -batch -Q \
  -L lisp \
  -l test/sumibi-romaji-to-hiragana-test.el \
  --eval "(ert-run-tests-interactively \"sumibi-romaji\")"
```

## テストの構成

### テストファイル
- `test/sumibi-romaji-to-hiragana-test.el` - ローマ字→ひらがな変換のテスト
- `test/sumibi-decisions-test.el` - Decisions非同期判定、古い応答の破棄、除外条件、候補・履歴・Undo、HTTPキャンセル・タイムアウトのテスト（APIキー不要・通信なし）

### Decisions方式のテスト

```sh
emacs -batch -Q -L lisp -l test/sumibi-decisions-test.el -f ert-run-tests-batch-and-exit
```

実APIを使うEmacs上の操作確認は [AMBIENT.md](AMBIENT.md#検証と制約) を参照してください。モックテストの通過だけでは実API・実操作の確認完了とはみなしません。

200ms集約については固定窓（後続打鍵で延長しない）、全入力状態の保持、IDによる回答対応、通信・変換中の次窓の保持、32状態を超える分割、HTTP回数上限、拒否・不正回答、設定変更・停止・タイムアウト、古い結果を適用しないことをモックで検証します。既存の単件送信テストは `sumibi-decisions-batch-window-ms=0` で維持しています。集約方式の実APIでのスコア独立性・遅延改善は別途確認が必要です。

### テストカテゴリ
1. **ローマ字変換テスト** - ローマ字からひらがなへの変換

## テストガイドライン

### 新しいテストの追加

```elisp
(ert-deftest sumibi-your-test ()
  "Test description."
  (let ((result (your-function "input")))
    (should (string= result "期待値"))))
```

### テストの命名規則

- **プレフィックス**: `sumibi-`
- **機能別**: `sumibi-romaji-*`

## トラブルシューティング

### よくある問題と解決方法

#### 1. 括弧の不整合エラー
```bash
# 修正後は必ず括弧チェックを実行
agent-lisp-paren-aid-linux lisp/sumibi.el
```

#### 2. 依存関係の問題
```bash
# 必要なパッケージがインストールされているか確認
emacs -batch -Q \
  --eval "(progn (require 'package) (package-initialize) (require 'dash))"
```

---

**注意**: テストファイルを編集した後は、必ず `agent-lisp-paren-aid-linux` で括弧の整合性を確認してください。
