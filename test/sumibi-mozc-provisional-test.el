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
;; 次のローマ字入力で mozc 仮確定を即確定 (期待動作)
;; ------------------------------------------------------------------

(defun sumibi-mozc-prov-test--inflight-overlays ()
  (seq-filter (lambda (o) (overlay-get o 'sumibi-mozc-inflight))
              (overlays-in (point-min) (point-max))))

(ert-deftest sumibi-mozc-prov-test-commit-on-next-input ()
  "次のローマ字入力で仮確定が mozc 結果で即確定し、後続 LLM は上書きしない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion) (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        ;; 次のローマ字を入力 -> commit フック相当
        (goto-char (point-max)) (insert "n")
        (sumibi--mozc-commit-pending-provisionals)
        (should (string= (buffer-string) "私はn"))
        (should (null (sumibi-mozc-prov-test--inflight-overlays)))
        ;; LLM 完了しても上書きされない
        (funcall (cdr capA))
        (funcall (car capA) "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}")
        (should (string= (buffer-string) "私はn"))))))

(ert-deftest sumibi-mozc-prov-test-commit-skips-when-mozc-failed ()
  "mozc が変換できなかった (灰色ローマ字) 仮確定は次入力で確定せず LLM を待つ。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion) (lambda (_r) nil))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        (goto-char (point-max)) (insert "n")
        (sumibi--mozc-commit-pending-provisionals)
        ;; ローマ字のまま、オーバーレイは残る (LLM 完了待ち)
        (should (string= (buffer-string) "watashihan"))
        (should (sumibi-mozc-prov-test--inflight-overlays))))))

;; ------------------------------------------------------------------
;; 案A (Issue #162): 変換直後の Ctrl-J で確定し、候補選択モードに入らない
;;   完了レース (仮確定の確定 vs 候補選択モード移行) の無害化。
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-async-sets-pending-flag ()
  "非同期変換の発火で `sumibi--mozc-async-pending' が t になる。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion) (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s _df _df2) nil)))
        (insert "watashiha")
        (setq sumibi--mozc-async-pending nil)
        (sumibi-henkan-region-async 1 10 nil)
        (should sumibi--mozc-async-pending)))))

(ert-deftest sumibi-mozc-prov-test-finalize-commits-provisional ()
  "pending 中の Ctrl-J (`sumibi-rK-trans') は mozc 仮確定を即確定し、
フラグをクリアし、候補選択モードには入らない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (sumibi-select-mode nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion) (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s _df _df2) nil)))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        (should sumibi--mozc-async-pending)
        (goto-char (point-max))
        (sumibi-rK-trans)
        (should (string= (buffer-string) "私は"))
        ;; カーソルは確定テキストの末尾 (先頭に飛ばない)。
        (should (= (point) (point-max)))
        (should (null sumibi--mozc-async-pending))
        (should (null (sumibi-mozc-prov-test--inflight-overlays)))
        (should-not sumibi-select-mode)))))

(ert-deftest sumibi-mozc-prov-test-finalize-after-completion-no-select-mode ()
  "完了でオーバーレイが消えた後 (フラグは残る) の Ctrl-J は、候補選択モードに
入らずフラグだけをクリアする (完了レースの無害化)。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (sumibi-select-mode nil))
      (insert "私は")
      (setq sumibi--mozc-async-pending t)
      (goto-char (point-max))
      (sumibi-rK-trans)
      (should (null sumibi--mozc-async-pending))
      (should-not sumibi-select-mode)
      (should (string= (buffer-string) "私は")))))

(ert-deftest sumibi-mozc-prov-test-pending-cleared-by-other-command ()
  "`sumibi-rK-trans' 以外のコマンド直前で投機確定フラグがクリアされ、
`sumibi-rK-trans' 直前では保持される。"
  (with-temp-buffer
    ;; 他コマンド (カーソル移動など) -> クリア
    (setq sumibi--mozc-async-pending t)
    (let ((this-command 'next-line))
      (sumibi--ambient-pre-command-cancel))
    (should (null sumibi--mozc-async-pending))
    ;; sumibi-rK-trans -> 保持 (確定動作のため残す)
    (setq sumibi--mozc-async-pending t)
    (let ((this-command 'sumibi-rK-trans))
      (sumibi--ambient-pre-command-cancel))
    (should sumibi--mozc-async-pending)))

;; ------------------------------------------------------------------
;; helper 不在時の通知 (機能有効時のみ一度だけ)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-warn-when-helper-missing ()
  "機能有効 + helper 不在のとき一度だけ message する。2回目は出さない。"
  (let ((sumibi-mozc-provisional-enable t)
        (sumibi--mozc-unavailable-warned nil)
        (count 0))
    (cl-letf (((symbol-function 'sumibi-mozc-available-p) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _args) (setq count (1+ count)))))
      (sumibi--mozc-warn-if-unavailable)
      (sumibi--mozc-warn-if-unavailable))
    (should (= count 1))
    (should sumibi--mozc-unavailable-warned)))

(ert-deftest sumibi-mozc-prov-test-no-warn-when-disabled ()
  "機能無効なら helper が無くても通知しない。"
  (let ((sumibi-mozc-provisional-enable nil)
        (sumibi--mozc-unavailable-warned nil)
        (count 0))
    (cl-letf (((symbol-function 'sumibi-mozc-available-p) (lambda () nil))
              ((symbol-function 'message)
               (lambda (&rest _args) (setq count (1+ count)))))
      (sumibi--mozc-warn-if-unavailable))
    (should (= count 0))
    (should-not sumibi--mozc-unavailable-warned)))

(ert-deftest sumibi-mozc-prov-test-warn-resets-when-available ()
  "helper が利用可能になれば通知フラグが解除される。"
  (let ((sumibi-mozc-provisional-enable t)
        (sumibi--mozc-unavailable-warned t))
    (cl-letf (((symbol-function 'sumibi-mozc-available-p) (lambda () t)))
      (sumibi--mozc-warn-if-unavailable))
    (should-not sumibi--mozc-unavailable-warned)))

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
;; 先頭 fence '/' の除去 (Issue #162: 仮確定で '/' が残るバグ)
;;   同期経路 `sumibi-henkan-region-sync' は先頭の '/' を変換対象に
;;   含めて削除するが、非同期 (仮確定) 経路で抜けており '/' が残っていた。
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-fence-slash-removed-on-llm ()
  "先頭 fence '/' は LLM 確定後にバッファへ残らない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq captured (cons df df2))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は")))
        (insert "/watashiha")               ; '/' は pos1、ローマ字は [2,11)
        (sumibi-henkan-region-async 2 11 nil)
        (funcall (cdr captured))
        (funcall (car captured)
                 "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
      ;; '/' が残らず "test" のみになる (バグ修正前は "/test")
      (should (string= (buffer-string) "test")))))

(ert-deftest sumibi-mozc-prov-test-fence-slash-removed-on-commit ()
  "先頭 fence '/' は仮確定の即確定 (次入力) 後にも残らない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion) (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "/watashiha")
        (sumibi-henkan-region-async 2 11 nil)
        (goto-char (point-max)) (insert "n")
        (sumibi--mozc-commit-pending-provisionals)
        ;; '/' が残らず "私はn" になる (バグ修正前は "/私はn")
        (should (string= (buffer-string) "私はn"))
        (should (null (sumibi-mozc-prov-test--inflight-overlays)))
        ;; LLM が後で完了しても上書きされない
        (funcall (cdr capA))
        (funcall (car capA) "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}")
        (should (string= (buffer-string) "私はn"))))))

;; ------------------------------------------------------------------
;; 確定後のカーソル位置 (Issue #162: 仮確定→確定でカーソルが先頭へ飛ぶバグ)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-cursor-at-end-after-confirm ()
  "仮確定→確定の直後、カーソルは挿入テキストの末尾に置かれる (先頭へ飛ばない)。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq captured (cons df df2))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は")))
        (insert "watashiha")           ; 領域 [1,10)、カーソルは末尾 (=10)
        (sumibi-henkan-region-async 1 10 nil)
        (funcall (cdr captured))        ; cleanup (領域削除)
        (funcall (car captured)         ; LLM 確定 "test" を挿入
                 "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
      (should (string= (buffer-string) "test"))
      ;; カーソルは "test" の末尾 (バグ修正前は先頭 =1 に飛んでいた)。
      (should (= (point) (point-max))))))

(ert-deftest sumibi-mozc-prov-test-cursor-preserved-when-typing-ahead ()
  "確定時にカーソルが領域外 (ambient 連続入力中) なら元の位置を保つ。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (captured nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq captured (cons df df2))))
                ((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は")))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)   ; 領域 [1,10)
        ;; ユーザーは領域の先で入力を続ける
        (goto-char (point-max)) (insert "X")    ; "watashihaX"、カーソルは X の直後
        (funcall (cdr captured))                ; cleanup
        (funcall (car captured)                 ; LLM 確定 "test" を挿入
                 "{\"choices\":[{\"message\":{\"content\":\"test\"}}]}"))
      ;; "watashiha"->"test" に置換され、ユーザーの "X" の直後にカーソルが残る
      (should (string= (buffer-string) "testX"))
      (should (= (point) (point-max)))
      (should (= (char-before (point)) ?X)))))

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
;; 複数同時進行時、先行変換の確定結果を後続オーバーレイが覆わない (表示バグ)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-overlay-does-not-swallow-completed-result ()
  "A 完了後、後続 B のオーバーレイ (display) が A の確定済みテキストを覆わない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil) (capB nil)
          (provs (list "私は" "日本語"))
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) (pop provs)))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2)
                   (if capA (setq capB (cons df df2)) (setq capA (cons df df2))))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)        ; 変換A [1,10)
        (goto-char (point-max)) (insert "nihongo")
        (sumibi-henkan-region-async 10 17 nil)       ; 変換B [10,17)
        ;; 変換A 完了 ("watashiha" -> "私は")
        (funcall (cdr capA))
        (funcall (car capA)
                 "{\"choices\":[{\"message\":{\"content\":\"%E7%A7%81%E3%81%AF\"}}]}")
        ;; 残る B のオーバーレイは A の結果「私は」(buffer 1-2) を覆っていないこと
        (let ((ov (car (overlays-in (point-min) (point-max)))))
          (should ov)
          (should (>= (overlay-start ov) 3)))
        ;; 変換B 完了で全確定・オーバーレイ無し
        (funcall (cdr capB))
        (funcall (car capB)
                 "{\"choices\":[{\"message\":{\"content\":\"%E6%97%A5%E6%9C%AC%E8%AA%9E\"}}]}")
        (should (string= (buffer-string) "私は日本語"))
        (should (null (overlays-in (point-min) (point-max))))))))

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

;; ------------------------------------------------------------------
;; 仮確定を第二候補に差し込む (Issue #162: 仮確定とLLM候補が揃ったら第二候補へ)
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-insert-second-basic ()
  "PROVISIONAL は第二候補 (index 1) に差し込まれる。"
  (should (equal (sumibi--insert-provisional-second '("a" "b" "c") "X")
                 '("a" "X" "b" "c"))))

(ert-deftest sumibi-mozc-prov-test-insert-second-single ()
  "候補が1件なら、その後ろ (第二候補) に差し込まれる。"
  (should (equal (sumibi--insert-provisional-second '("a") "X")
                 '("a" "X"))))

(ert-deftest sumibi-mozc-prov-test-insert-second-empty-list ()
  "候補が空なら PROVISIONAL のみのリストになる。"
  (should (equal (sumibi--insert-provisional-second '() "X")
                 '("X"))))

(ert-deftest sumibi-mozc-prov-test-insert-second-dedup ()
  "PROVISIONAL が既に候補に含まれていれば差し込まない (重複回避)。"
  (should (equal (sumibi--insert-provisional-second '("a" "b") "a")
                 '("a" "b")))
  (should (equal (sumibi--insert-provisional-second '("a" "b") "b")
                 '("a" "b"))))

(ert-deftest sumibi-mozc-prov-test-insert-second-nil-or-empty ()
  "PROVISIONAL が nil や空文字なら差し込まず元のリストを返す。"
  (should (equal (sumibi--insert-provisional-second '("a" "b") nil)
                 '("a" "b")))
  (should (equal (sumibi--insert-provisional-second '("a" "b") "")
                 '("a" "b"))))

(ert-deftest sumibi-mozc-prov-test-async-second-candidate-is-provisional ()
  "非同期成功後、第二候補が mozc 仮確定文字列になる (Issue #162)。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current "わたしは")  ; LLM 結果と異なる仮確定
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
      ;; 確定テキストは LLM 結果
      (should (string= (buffer-string) "私は"))
      ;; 第1候補は LLM 結果、第2候補は mozc 仮確定
      (should (string= (car (nth 0 sumibi-henkan-kouho-list)) "私は"))
      (should (string= (car (nth 1 sumibi-henkan-kouho-list)) "わたしは")))))

(ert-deftest sumibi-mozc-prov-test-async-second-candidate-dedup ()
  "仮確定が LLM 第1候補と同一なら第二候補に重複して差し込まない (Issue #162)。"
  (with-temp-buffer
    (let ((sumibi--mozc-provisional-current "私は")  ; LLM 結果と同一
          (sumibi-henkan-kouho-list nil)
          (sumibi-markers nil)
          (sumibi-genbun nil)
          (sumibi-history-stack nil))
      (sumibi-mozc-prov-test--with-stubbed-post
          "{\"choices\":[{\"message\":{\"content\":\"%E7%A7%81%E3%81%AF\"}}]}"
        (goto-char (point-min))
        (sumibi-roman-to-kanji-with-surrounding "watashiha" "watashiha" 1
                                                (lambda () nil)))
      (should (string= (buffer-string) "私は"))
      ;; 第1候補は「私は」、その直後 (第2候補) は「原文まま」(仮確定は重複なので入らない)
      (should (string= (car (nth 0 sumibi-henkan-kouho-list)) "私は"))
      (should-not (string= (car (nth 1 sumibi-henkan-kouho-list)) "私は")))))

;; ------------------------------------------------------------------
;; 早期確定後の候補選択 (Issue #162)
;;   type-ahead / Ctrl-J で仮確定を早期確定した後に LLM が完了したら、
;;   本文は上書きせず「mozc確定 → LLM結果 → 原文」の候補選択状態を構築する。
;; ------------------------------------------------------------------

(ert-deftest sumibi-mozc-prov-test-early-commit-builds-candidates ()
  "早期確定後に LLM が完了すると、mozc確定を第1候補・LLM結果を第2候補とする
候補選択状態が構築され、本文は mozc 確定のまま上書きされない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-early-commit-region nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "ディストリビュー村"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "dhisutoribyu-son")
        (sumibi-henkan-region-async 1 17 nil)
        ;; type-ahead: 次のキー入力で早期確定
        (goto-char (point-max)) (insert " ")
        (sumibi--mozc-commit-pending-provisionals)
        (should (string= (buffer-string) "ディストリビュー村 "))
        ;; LLM 完了 (cleanup → insert)。結果は「ディストリビューション」
        (funcall (cdr capA))
        (funcall (car capA)
                 "{\"choices\":[{\"message\":{\"content\":\"%E3%83%87%E3%82%A3%E3%82%B9%E3%83%88%E3%83%AA%E3%83%93%E3%83%A5%E3%83%BC%E3%82%B7%E3%83%A7%E3%83%B3\"}}]}")
        ;; 本文は mozc 確定のまま (LLM 結果で上書きされない)
        (should (string= (buffer-string) "ディストリビュー村 "))
        ;; 候補: 第1候補 = mozc 確定、第2候補 = LLM 結果、末尾 = 原文まま
        (should (string= (car (nth 0 sumibi-henkan-kouho-list)) "ディストリビュー村"))
        (should (string= (car (nth 1 sumibi-henkan-kouho-list)) "ディストリビューション"))
        (should (equal (nth 1 (car (last sumibi-henkan-kouho-list))) "原文まま"))
        ;; 確定文字列上にカーソルを置くと履歴から候補状態を発見できる
        ;; (Ctrl-J の再変換で使われる)。`sumibi-history-search' は引数では
        ;; なく現在の point を参照するため、カーソルを移動して確認する。
        (save-excursion
          (goto-char 2)
          (should (sumibi-history-search 2 nil)))
        ;; 受け渡し用のグローバルは消費後にクリアされる
        (should (null sumibi--mozc-early-commit-region))))))

(ert-deftest sumibi-mozc-prov-test-early-commit-candidates-dedup ()
  "LLM 結果が mozc 確定と同一なら候補に重複して並べない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-early-commit-region nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        (goto-char (point-max)) (insert "n")
        (sumibi--mozc-commit-pending-provisionals)
        (should (string= (buffer-string) "私はn"))
        ;; LLM 完了。結果は mozc 確定と同じ「私は」
        (funcall (cdr capA))
        (funcall (car capA)
                 "{\"choices\":[{\"message\":{\"content\":\"%E7%A7%81%E3%81%AF\"}}]}")
        (should (string= (buffer-string) "私はn"))
        ;; 第1候補は「私は」、第2候補に重複の「私は」は入らない
        (should (string= (car (nth 0 sumibi-henkan-kouho-list)) "私は"))
        (should-not (string= (car (nth 1 sumibi-henkan-kouho-list)) "私は"))))))

(ert-deftest sumibi-mozc-prov-test-early-commit-llm-error-no-candidates ()
  "早期確定後に LLM が失敗した場合は候補状態を作らず、本文も変えない。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-early-commit-region nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        (goto-char (point-max)) (insert "n")
        (sumibi--mozc-commit-pending-provisionals)
        (should (string= (buffer-string) "私はn"))
        ;; LLM 失敗
        (funcall (cdr capA))
        (funcall (car capA) "{\"error\":{\"message\":\"TIMEOUT ERROR\"}}")
        ;; 本文は mozc 確定のまま、候補状態は構築されない
        (should (string= (buffer-string) "私はn"))
        (should (null sumibi-henkan-kouho-list))
        (should (null sumibi--mozc-early-commit-region))))))

(ert-deftest sumibi-mozc-prov-test-user-edit-no-early-commit-state ()
  "ユーザーが仮確定領域を手で編集した場合は早期確定扱いにならない
(候補状態を構築しない)。"
  (with-temp-buffer
    (let ((sumibi-mozc-provisional-enable t)
          (capA nil)
          (sumibi--mozc-cancel-overwrite nil)
          (sumibi--mozc-early-commit-region nil)
          (sumibi--mozc-provisional-current nil)
          (sumibi-genbun nil) (sumibi-markers nil)
          (sumibi-henkan-kouho-list nil) (sumibi-history-stack nil))
      (cl-letf (((symbol-function 'sumibi-mozc-provisional-conversion)
                 (lambda (_r) "私は"))
                ((symbol-function 'sumibi-openai-http-post)
                 (lambda (_m _n _s df df2) (setq capA (cons df df2)))))
        (insert "watashiha")
        (sumibi-henkan-region-async 1 10 nil)
        ;; ユーザーが領域を編集 (早期確定ではない)
        (goto-char 10) (delete-char -2)
        (funcall (cdr capA))
        (funcall (car capA)
                 "{\"choices\":[{\"message\":{\"content\":\"%E7%A7%81%E3%81%AF\"}}]}")
        ;; 編集が尊重され、候補状態は構築されない
        (should (string= (buffer-string) "watashi"))
        (should (null sumibi-henkan-kouho-list))
        (should (null sumibi--mozc-early-commit-region))))))

(provide 'sumibi-mozc-provisional-test)

;;; sumibi-mozc-provisional-test.el ends here
