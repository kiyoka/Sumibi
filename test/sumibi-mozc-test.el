;;; sumibi-mozc-test.el --- Tests for sumibi-mozc.el -*- lexical-binding: t; -*-
;;
;; mozc_emacs_helper クライアント (Issue #162) の ERT テスト。
;;
;; mozc_emacs_helper が見つからない環境では、実変換を伴うテストは
;; `skip-unless' でスキップされる (パス検出・空入力など helper を必要と
;; しないテストは常に実行される)。
;;
;;; Code:

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory (or load-file-name buffer-file-name))))
(require 'ert)
(require 'sumibi-mozc)

;; ------------------------------------------------------------------
;; helper を必要としないテスト
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-test-available-p-returns-boolean ()
  "`sumibi-mozc-available-p' は常に t か nil を返す。"
  (should (memq (sumibi-mozc-available-p) '(t nil))))

(ert-deftest sumibi-mozc-test-quote-key-plain ()
  "通常文字は \"x\" 形式でクォートされる。"
  (should (string= (sumibi-mozc--quote-key ?あ) "\"あ\"")))

(ert-deftest sumibi-mozc-test-quote-key-doublequote ()
  "ダブルクォートはエスケープされる。"
  (should (string= (sumibi-mozc--quote-key ?\") "\"\\\"\"")))

(ert-deftest sumibi-mozc-test-quote-key-backslash ()
  "バックスラッシュはエスケープされる。"
  (should (string= (sumibi-mozc--quote-key ?\\) "\"\\\\\"")))

(ert-deftest sumibi-mozc-test-empty-input-returns-nil ()
  "空文字列の変換は nil を返す (helper 不要)。"
  (should (null (sumibi-mozc-convert ""))))

(ert-deftest sumibi-mozc-test-non-string-returns-nil ()
  "文字列以外の入力は nil を返す。"
  (should (null (sumibi-mozc-convert nil))))

(ert-deftest sumibi-mozc-test-helper-path-override ()
  "`sumibi-mozc-helper-path' に存在しないパスを与えても find-helper は壊れない。"
  (let ((sumibi-mozc-helper-path "/nonexistent/mozc_emacs_helper"))
    ;; 指定が無効でも候補からのフォールバックで nil もしくは候補パスを返す
    (should (or (null (sumibi-mozc-find-helper))
                (stringp (sumibi-mozc-find-helper))))))

;; ------------------------------------------------------------------
;; helper を必要とするテスト (mozc 未導入ならスキップ)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-test-convert-watashiha ()
  "「わたしは」が「私は」に変換される。"
  (skip-unless (sumibi-mozc-available-p))
  (unwind-protect
      (should (string= (sumibi-mozc-convert "わたしは") "私は"))
    (sumibi-mozc-shutdown)))

(ert-deftest sumibi-mozc-test-convert-reuses-process ()
  "連続変換でプロセスが再利用され、複数回正しく変換できる。"
  (skip-unless (sumibi-mozc-available-p))
  (unwind-protect
      (progn
        (should (string= (sumibi-mozc-convert "とうきょうで") "東京で"))
        (let ((proc-after-first sumibi-mozc--process)
              (session-after-first sumibi-mozc--session-id))
          (should (string= (sumibi-mozc-convert "にほんご") "日本語"))
          ;; 同一プロセス・同一セッションが使い回されている
          (should (eq proc-after-first sumibi-mozc--process))
          (should (equal session-after-first sumibi-mozc--session-id))))
    (sumibi-mozc-shutdown)))

(ert-deftest sumibi-mozc-test-shutdown-clears-state ()
  "shutdown 後はプロセス・セッションが nil に初期化される。"
  (skip-unless (sumibi-mozc-available-p))
  (sumibi-mozc-convert "あ")
  (sumibi-mozc-shutdown)
  (should (null sumibi-mozc--process))
  (should (null sumibi-mozc--session-id)))

(provide 'sumibi-mozc-test)

;;; sumibi-mozc-test.el ends here
