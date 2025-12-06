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
