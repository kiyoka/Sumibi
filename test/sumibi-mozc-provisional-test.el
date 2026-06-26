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
  (defun -zip-pair (l1 l2) (cl-mapcar #'cons l1 l2))
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

;; ------------------------------------------------------------------
;; 点8: LLM 待ち中の編集で上書きをキャンセル
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-cancel-overwrite-on-edit ()
  "LLM 待ち中に仮確定領域が編集されたら、結果で上書きせずユーザーの編集を残す (点8)。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured nil)
          (sumibi--mozc-cancel-overwrite nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq captured (cons df df2))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は")))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)   ; 領域 [1,10)="watashiha"
        ;; ユーザーが領域を編集: 末尾 "ha" を削除 → "watashi"
        (goto-char 10)
        (delete-char -2)
        ;; コールバック発火 (cleanup → insert)
        (funcall (cdr captured))
        (funcall (car captured)
                 "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
      ;; 上書きがキャンセルされ、編集後のテキストが残る
      (should (string= (buffer-string) "watashi")))))

(ert-deftest sumibi-mozc-prov-test-no-edit-allows-overwrite ()
  "編集が無ければ通常どおり LLM 結果で上書きされる (点8 の対照)。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured nil)
          (sumibi--mozc-cancel-overwrite nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq captured (cons df df2))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は")))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        ;; 編集せずにコールバック発火
        (funcall (cdr captured))
        (funcall (car captured)
                 "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
      (should (string= (buffer-string) "test")))))

;; ------------------------------------------------------------------
;; 非同期挿入のバッファ文脈 (レビュー指摘 #2/#3/#4)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-insert-targets-original-buffer ()
  "current-buffer が変わっても結果は元バッファに挿入される (#2)。"
  (let ((buf-a (generate-new-buffer " *sumibi-test-a*"))
        (buf-b (generate-new-buffer " *sumibi-test-b*"))
        (captured nil)
        (sumibi--mozc-cancel-overwrite nil)
        (sumibi--mozc-provisional-current nil)
        (sumibi-genbun nil) (sumibi-markers nil)
        (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
    (unwind-protect
        (progn
          (with-current-buffer buf-a (insert "watashiha"))
          (cl-letf (((symbol-function 'sumibi-openai-http-post)
                     (lambda (_m _n _s df df2) (setq captured (cons df df2)))))
            (with-current-buffer buf-a
              (goto-char (point-min))
              (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                      (lambda () nil)))
            ;; current-buffer を B に切り替えてから挿入コールバックを発火
            (set-buffer buf-b)
            (funcall (car captured)
                     "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
          ;; 結果は A に挿入され、B は変化しない
          (should (string= (with-current-buffer buf-a (buffer-string)) "testwatashiha"))
          (should (string= (with-current-buffer buf-b (buffer-string)) "")))
      (kill-buffer buf-a)
      (kill-buffer buf-b))))

(ert-deftest sumibi-mozc-prov-test-insert-handles-killed-buffer ()
  "元バッファが kill されても挿入コールバックはクラッシュしない (#3)。"
  (let ((buf-a (generate-new-buffer " *sumibi-test-a*"))
        (captured nil)
        (sumibi--mozc-cancel-overwrite nil)
        (sumibi--mozc-provisional-current nil))
    (with-current-buffer buf-a (insert "watashiha"))
    (cl-letf (((symbol-function 'sumibi-openai-http-post)
               (lambda (_m _n _s df df2) (setq captured (cons df df2)))))
      (with-current-buffer buf-a
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (kill-buffer buf-a)
      ;; kill されたバッファのマーカー経由でもエラーにならず素通りする
      (funcall (car captured)
               "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
    ;; ここに到達すればクラッシュしていない
    (should t)))

;; ------------------------------------------------------------------
;; #3: 非同期成功後の候補選択状態の構築
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-async-builds-candidate-state ()
  "非同期成功後に候補選択状態 (kouho-list/markers) が構築される (#3)。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current nil)
          (sumibi-henkan-kouho-list nil)
          (sumibi-markers nil)
          (sumibi-genbun nil)
          (sumibi-history-stack nil))
      ;; content は URL-hex エンコードされた UTF-8 「私は」
      (sumibi-mozc-prov-test--with-stubbed-post
          "{\"choices\":[{\"message\":{\"content\":\"%E7%A7%81%E3%81%AF\"}}]}"
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (should (string= (buffer-string) "私は"))
      ;; 候補リストが構築されている (本命 + 原文まま)
      (should (>= (length sumibi-henkan-kouho-list) 2))
      ;; 第1候補が確定テキスト
      (should (string= (car (nth 0 sumibi-henkan-kouho-list)) "私は"))
      ;; markers が設定されている (確定後の再変換ポップアップに必要)
      (should (markerp (car sumibi-markers)))
      (should (markerp (cdr sumibi-markers))))))

;; ------------------------------------------------------------------
;; 点6: 進行中領域との重複防止クランプ
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-clamp-no-overlay ()
  "in-flight オーバーレイが無ければ開始位置は変わらない。"
  (with-temp-buffer
    (insert "watashiha")
    (should (= (sumibi--mozc-clamp-start 1 10) 1))))

(ert-deftest sumibi-mozc-prov-test-clamp-with-inflight ()
  "in-flight オーバーレイがあれば開始位置をその終端まで前進させる。"
  (with-temp-buffer
    (insert "watashihanihonni")
    (let ((ov (make-overlay 1 10)))
      (overlay-put ov 'sumibi-mozc-inflight t)
      ;; [1,10) が in-flight なので [1,17) の変換開始は 10 にクランプされる
      (should (= (sumibi--mozc-clamp-start 1 17) 10)))))

(ert-deftest sumibi-mozc-prov-test-clamp-ignores-unmarked-overlay ()
  "印の無いオーバーレイはクランプ対象にしない。"
  (with-temp-buffer
    (insert "watashihanihonni")
    (make-overlay 1 10)  ; sumibi-mozc-inflight 印なし
    (should (= (sumibi--mozc-clamp-start 1 17) 1))))

;; ------------------------------------------------------------------
;; 点6: フォールバック値の変換ごとの独立性 (クロージャ捕捉)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-fallback-is-per-conversion ()
  "別の変換がグローバル値を上書きしても、各変換は自分の仮確定を確定する。"
  (with-temp-buffer
    ;; グローバル変数はテスト後に復元する (let で束縛)
    (let ((captured nil)
          (sumibi--mozc-provisional-current nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s deferred-func deferred-func2)
                   (setq captured (cons deferred-func deferred-func2)))))
        ;; 変換A: 仮確定 "私は" を設定して呼ぶ (スタブはコールバックを保留)
        (setq sumibi--mozc-provisional-current "私は")
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil))
        ;; 変換Bが始まってグローバルを上書きしたと仮定
        (setq sumibi--mozc-provisional-current "日本語")
        ;; ここでAのコールバックがエラーで発火 → Aは自分の "私は" を確定すべき
        (funcall (cdr captured))
        (funcall (car captured) "{\"error\":{\"message\":\"X\"}}"))
      (should (string= (buffer-string) "私は")))))

;; ------------------------------------------------------------------
;; 点6: 隣接する進行中領域のマーカー境界
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-adjacent-inflight-regions ()
  "隣接する2変換が同時進行しても、先行変換の確定結果を後続が取り込まない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured '())
          (prov-seq (list "私は" "日本に")))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s deferred-func deferred-func2)
                   (setq captured (append captured
                                          (list (cons deferred-func deferred-func2))))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_romaji) (pop prov-seq))))
        (insert "watashihanihonni")
        (sumibi-henkan-region-async 1 10 nil)    ; A: [1,10) "watashiha"
        (sumibi-henkan-region-async 10 17 nil)   ; B: [10,17) "nihonni"
        (let ((a (nth 0 captured))
              (b (nth 1 captured))
              (err "{\"error\":{\"message\":\"X\"}}"))
          ;; 実際の deferred 順 (後始末 → 挿入) を A, B の順で再現
          (funcall (cdr a)) (funcall (car a) err)
          (funcall (cdr b)) (funcall (car b) err)))
      (should (string= (buffer-string) "私は日本に")))))

(provide 'sumibi-mozc-provisional-test)

;;; sumibi-mozc-provisional-test.el ends here
