;;; sumibi-mozc-provisional-test.el --- Tests for mozc provisional conversion -*- lexical-binding: t; -*-
;;
;; Issue #162 (点4/点5) のテスト。
;;   点4: mozc 仮確定が有効なとき、対象となる短文を非同期変換に回す判定
;;        (`sumibi--mozc-force-async-p' / `sumibi--fixed-kouho-p')。
;;   点5: LLM 失敗/タイムアウト時に mozc 仮確定結果を最終結果として確定する
;;        (`sumibi-roman-to-kanji-with-surrounding' のフォールバック)。
;;
;;; Code:

(require 'ert)
(require 'cl-lib)

;; 依存が無い環境向けの最小スタブ (他テストと同方針)
(unless (require 'popup nil 'noerror)
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))
(unless (require 'unicode-escape nil 'noerror)
  (defun unicode-escape (s) s)
  (defun unicode-escape-to-string (s) s)
  (provide 'unicode-escape))
(unless (require 'deferred nil 'noerror)
  (defun deferred:succeed (&rest _args) nil)
  (defun deferred:nextc (&rest _args) nil)
  (defun deferred:error (&rest _args) nil)
  (provide 'deferred))
(unless (require 'dash nil 'noerror)
  (defun -filter (fn list)
    (let (result)
      (dolist (item list) (when (funcall fn item) (push item result)))
      (nreverse result)))
  (defun -map (fn list) (mapcar fn list))
  (provide 'dash))

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory (or load-file-name buffer-file-name))))
(require 'sumibi)

;; ------------------------------------------------------------------
;; 点4: 固定変換キーワード判定
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-fixed-kouho-particle ()
  "助詞 \"wo\" は固定変換キーワードと判定される。"
  (should (sumibi--fixed-kouho-p "wo")))

(ert-deftest sumibi-mozc-prov-test-fixed-kouho-word-is-not-fixed ()
  "通常のローマ字語は固定変換キーワードではない。"
  (should-not (sumibi--fixed-kouho-p "watashiha")))

;; ------------------------------------------------------------------
;; 点4: 非同期化すべきかの判定
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-force-async-disabled ()
  "機能無効時は常に nil。"
  (let ((sumibi-mozc-provisional-enable nil))
    (should-not (sumibi--mozc-force-async-p "watashiha" nil))))

(ert-deftest sumibi-mozc-prov-test-force-async-romaji ()
  "有効時、ローマ字短文は非同期化対象 (mozc 必要)。"
  (skip-unless (sumibi-mozc-available-p))
  (let ((sumibi-mozc-provisional-enable t))
    (should (sumibi--mozc-force-async-p "watashiha" nil))))

(ert-deftest sumibi-mozc-prov-test-force-async-skips-kanji ()
  "漢字を含む入力は非同期化対象外。"
  (let ((sumibi-mozc-provisional-enable t))
    (should-not (sumibi--mozc-force-async-p "私は" nil))))

(ert-deftest sumibi-mozc-prov-test-force-async-skips-fixed ()
  "固定変換キーワードは非同期化対象外。"
  (let ((sumibi-mozc-provisional-enable t))
    (should-not (sumibi--mozc-force-async-p "wo" nil))))

(ert-deftest sumibi-mozc-prov-test-force-async-skips-inverse ()
  "逆変換 (日→英) は非同期化対象外。"
  (let ((sumibi-mozc-provisional-enable t))
    (should-not (sumibi--mozc-force-async-p "watashiha" t))))

;; ------------------------------------------------------------------
;; 点5: LLM 失敗時のフォールバック
;; ------------------------------------------------------------------

(defmacro sumibi-mozc-prov-test--with-stubbed-post (response-json &rest body)
  "`sumibi-openai-http-post' をスタブし、非同期コールバックに RESPONSE-JSON を
渡して同期的に実行する文脈で BODY を評価する。"
  (declare (indent 1))
  `(cl-letf (((symbol-function 'sumibi-openai-http-post)
              (lambda (_msgs _n _sync deferred-func deferred-func2)
                (when deferred-func2 (funcall deferred-func2))
                (funcall deferred-func ,response-json))))
     ,@body))

(ert-deftest sumibi-mozc-prov-test-fallback-on-error ()
  "LLM がエラーを返し、仮確定があれば仮確定結果が確定される。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current "私は"))
      (sumibi-mozc-prov-test--with-stubbed-post
          "{\"error\":{\"message\":\"TIMEOUT ERROR\"}}"
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (should (string= (buffer-string) "私は")))))

(ert-deftest sumibi-mozc-prov-test-error-without-provisional ()
  "仮確定が無ければ、従来通りエラー文字列が挿入される。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current nil))
      (sumibi-mozc-prov-test--with-stubbed-post
          "{\"error\":{\"message\":\"TIMEOUT ERROR\"}}"
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (should (string= (buffer-string) "!!TIMEOUT ERROR!!")))))

(ert-deftest sumibi-mozc-prov-test-success-ignores-provisional ()
  "LLM が成功した場合は、仮確定があっても LLM 結果が挿入される。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current "私は"))
      (sumibi-mozc-prov-test--with-stubbed-post
          "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (should (string= (buffer-string) "test")))))

(provide 'sumibi-mozc-provisional-test)

;;; sumibi-mozc-provisional-test.el ends here
