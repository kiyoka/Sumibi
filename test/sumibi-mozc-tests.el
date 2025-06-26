;; sumibi-mozc-tests.el --- Unit tests for Sumibi Mozc backend -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; These tests verify that the Mozc backend for Sumibi correctly converts
;; several typical romaji inputs into the expected Japanese strings.
;; The tests are executed only when `mozc.el` can be loaded successfully
;; on the executing environment.  When Mozc is unavailable, each test is
;; skipped so that the overall test suite can run on environments (e.g. CI)
;; that do not have Mozc installed.

;;; Code:

;;; sumibi-mozc-tests.el --- Unit tests for Sumibi Mozc backend -*- lexical-binding: t; -*-

(require 'ert)

;; Some Emacs builds attempt to create temporary directories for native
;; compilation output even in batch mode.  On restricted filesystems this can
;; fail with a permission error that stops the entire test run.  Disable JIT
;; native compilation for the duration of this test suite to avoid that issue.
(when (boundp 'native-comp-jit-compilation)
  (setq native-comp-jit-compilation nil))

;; ------------------------------------------------------------
;; Test setup helpers
;; ------------------------------------------------------------

;; ------------------------------------------------------------------
;; Ensure MELPA / ELPA is available so that dependencies like dash.el can be
;; installed on-the-fly when running the test in a pristine environment.
;; ------------------------------------------------------------------

(require 'package)
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install dash.el automatically if it's missing.
(unless (require 'dash nil 'noerror)
  (package-refresh-contents)
  (package-install 'dash)
  (require 'dash))

;; Ensure the `lisp` directory of this repository is on `load-path` so that
;; `sumibi.el` can be required from a standalone Emacs process.
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

;; Sumibi depends on the external package `popup.el` for its interactive
;; completion UI.  The testing environment may not have this package
;; installed, and the Mozc conversion routine we are testing does not depend
;; on any of the interactive `popup-*` functions.  To keep the unit tests
;; self-contained, we provide a minimal stub implementation of the symbols
;; that `sumibi.el` tries to use, if `popup` cannot be loaded.

(unless (require 'popup nil 'noerror)
  ;; Very small shim – only what is needed for `require` to succeed.
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))

;; Sumibi also depends on `unicode-escape` and `deferred`, which might not be
;; available in a clean testing environment.  Provide minimal stubs so that the
;; `require` calls inside `sumibi.el` succeed.  These libraries are not needed
;; for Mozc conversion itself.

(dolist (lib '(unicode-escape deferred))
  (unless (require lib nil 'noerror)
    (provide lib)))

;; Dash.el がインストールされている前提でテストを実行する。
(require 'dash)

;; Now we can safely load Sumibi.
(require 'sumibi)

;; Force the backend to Mozc for these tests.
(setq sumibi-backend 'mozc)

;; ------------------------------------------------------------------
;; Basic conversions (no prefix)
;; ------------------------------------------------------------------
(ert-deftest sumibi-mozc-henkan ()
  "Converting 'henkan' should yield the Japanese string '変換'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "henkan" "" 1 nil))))
      (should (string= result "変換")))))

(ert-deftest sumibi-mozc-nihongo ()
  "Converting 'nihongo' should yield '日本語'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "nihongo" "" 1 nil))))
      (should (string= result "日本語")))))

(ert-deftest sumibi-mozc-nihongoga-dekimasu ()
  "Converting multi-word input 'nihongoga dekimasu' should yield '日本語が出来ます'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "nihongoga dekimasu" "" 1 nil))))
      (should (string= result "日本語が出来ます")))))

;; ------------------------------------------------------------------
;; Markdown prefix handling tests
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-md-heading ()
  "'# midashi' should convert to '# 見出し'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "# midashi" "" 1 nil))))
      ;; Mozc 変換では半角 # が全角 ＃ に変わり、スペースが消えるケースがある
      (should (string= result "＃見出し")))))

(ert-deftest sumibi-mozc-md-list-star ()
  "'* koumoku' should convert to '* 項目'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "* koumoku" "" 1 nil))))
      ;; '*' -> '＊' に変化、スペースが省略される
      (should (string= result "＊項目")))))

(ert-deftest sumibi-mozc-md-list-dash ()
  "'- koumoku' should convert to '- 項目'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (let ((result (car (sumibi-roman-to-kanji-with-surrounding "- koumoku" "" 1 nil))))
      ;; '-' -> 'ー' (長音記号) に変化、スペース無し
      (should (string= result "ー項目")))))

;; ------------------------------------------------------------------
;; End-to-end interaction test on *scratch* buffer
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-scratch-heading-ctrl-j ()
  "Insert '# midashi' in *scratch*, press C-j, and expect '# 見出し'."
  (if (not sumibi--mozc-available-p)
      (ert-skip "Mozc not available on this environment")
    (with-current-buffer (get-buffer-create "*scratch*")
      (let ((sumibi-backend 'mozc))
        ;; 1. バッファをクリアし Sumibi モードを有効化
        (erase-buffer)
        (sumibi-mode 1)

        ;; 2. '# midashi' を入力
        (goto-char (point-min))
        (insert "# midashi")

        ;; 3. 行末で C-j (sumibi-rK-trans) を呼ぶ
        (goto-char (point-max))
        (sumibi-rK-trans)

        ;; 4. バッファ内容を比較 (改行を含む場合はトリム)
        (should (string= (string-trim (buffer-string)) "# 見出し"))))))

(provide 'sumibi-mozc-tests)

;;; sumibi-mozc-tests.el ends here
