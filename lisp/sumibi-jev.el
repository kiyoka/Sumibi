;;; sumibi-jev.el --- Nonblocking ambient conversion decisions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Loaded by sumibi.el.  Jev decides when to convert, not how to convert.
;; There is at most one decision/conversion request per buffer.  Only the
;; newest queued observation is retained; obsolete responses never edit text.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-http)
(require 'subr-x)
(defvar url-http-response-status)

(defcustom sumibi-jev-threshold 0.8
  "Jev評価値がこの値以上なら変換する。"
  :type 'number :group 'sumibi)
(defcustom sumibi-jev-debug t
  "Jevの動作ログを*sumibi-debug*に出力する。入力本文やAPIキーは記録しない。"
  :type 'boolean :group 'sumibi)
(defcustom sumibi-jev-model "jev-latest"
  "Jev判定に使用するモデル名。"
  :type 'string :group 'sumibi)
(defcustom sumibi-jev-timeout 10
  "Jev判定のタイムアウト秒数。失敗時は入力を変更しない。"
  :type 'number :group 'sumibi)
(defcustom sumibi-jev-max-context-length 512
  "判定・変換対象の最大文字数。超える入力は手動変換に任せる。"
  :type 'integer :group 'sumibi)
(defcustom sumibi-jev-max-calls-per-buffer 1000
  "バッファ当たりのJev判定リクエスト上限。変換APIの料金は別途発生する。"
  :type 'integer :group 'sumibi)

(defconst sumibi-jev-endpoint "https://api.typesafe.ai/v1/systemone")
(defconst sumibi--jev-question
  '((type . "noul")
    (instructions . "At the current observation point, should a Japanese romaji input method start automatic conversion of the text before the cursor? A space after a Japanese particle (for example wa, ga, wo, ni, de, ha, no) is a useful conversion point even if the sentence may continue. A space after a complete Japanese utterance is also a conversion point. A period, comma, or question mark is a conversion point only after a pause of at least 500 milliseconds. Judge the text and latest key together; do not predict future typing.")
    (criteria . ((true . "The latest key completed a plausible Japanese romaji phrase at a space, or punctuation was followed by a pause of at least 500 ms.")
                 (false . "Still inside a word or phrase, a quick punctuation continuation, English prose, code, a number, or a non-Japanese editing context."))))
  "Issue #187で検証した質問文v2。")

(defvar-local sumibi--jev-generation 0)
(defvar-local sumibi--jev-pending nil)
(defvar-local sumibi--jev-inflight nil)
(defvar-local sumibi--jev-cancel-request nil)
(defvar-local sumibi--jev-timer nil)
(defvar-local sumibi--jev-call-count 0)
(defvar-local sumibi--jev-last-error nil)
(defvar-local sumibi--jev-pre-command-tick nil)
(defvar-local sumibi--jev-observed-generation nil)

(defun sumibi--jev-log (format-string &rest args)
  "Log a sanitized Jev event using FORMAT-STRING and ARGS."
  (when sumibi-jev-debug
    (let ((line (concat (format-time-string "%H:%M:%S ") "[Jev] "
                        (apply #'format format-string args) "\n")))
      (with-current-buffer (get-buffer-create "*sumibi-debug*")
        (let ((inhibit-read-only t) (inhibit-modification-hooks t))
          (save-excursion (goto-char (point-max)) (insert line)))))))

(defun sumibi-jev-status ()
  "Show current buffer's Jev state without exposing credentials or input."
  (interactive)
  (let ((key (getenv "TYPESAFE_API_KEY")))
    (message "Jev: mode=%s enabled=%s backend=%s allowed=%s key-set=%s calls=%d/%d phase=%s pending=%s timer=%s error=%s"
             sumibi-mode sumibi-ambient-enable sumibi-ambient-backend
             (sumibi--jev-allowed-p) (and key (not (string-empty-p key)))
             sumibi--jev-call-count sumibi-jev-max-calls-per-buffer
             (plist-get sumibi--jev-inflight :phase)
             (not (null sumibi--jev-pending)) (not (null sumibi--jev-timer))
             sumibi--jev-last-error)))

(defun sumibi--jev-http-post (url headers body timeout callback)
  "POST URL asynchronously with HEADERS and BODY; return a cancellation function.
CALLBACK receives response JSON text and a sanitized error symbol.
TIMEOUT includes the entire operation.  Cancellation does not invoke CALLBACK."
  (let ((url-request-method "POST")
        (url-request-extra-headers headers)
        (url-request-data body)
        (url-show-status nil)
        (url-privacy-level 'paranoid)
        (url-http-attempt-keepalives nil)
        response timer done)
    (cl-labels
        ((cleanup ()
           (when timer (cancel-timer timer))
           (when (buffer-live-p response)
             (let ((process (get-buffer-process response)))
               (when (process-live-p process) (delete-process process)))
             (kill-buffer response)))
         (finish (text error)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback text error))))
      (condition-case nil
          (progn
            (setq response
                  (url-retrieve
                   url
                   (lambda (status)
                     (let ((buf (current-buffer)) text failure)
                       (unless done
                         (setq response buf)
                         (condition-case nil
                             (if (or (plist-get status :error)
                                     (not (and (integerp url-http-response-status)
                                               (<= 200 url-http-response-status)
                                               (< url-http-response-status 300))))
                                 (setq failure 'http-error)
                               (goto-char (point-min))
                               (unless (re-search-forward "\r?\n\r?\n" nil t)
                                 (error "Missing HTTP headers"))
                               (setq text (decode-coding-string
                                           (buffer-substring-no-properties (point) (point-max))
                                           'utf-8)))
                           (error (setq failure 'invalid-response)))
                         (finish text failure))
                       (when (buffer-live-p buf) (kill-buffer buf))))
                   nil t t))
            (unless done
              (if (buffer-live-p response)
                  (setq timer (run-with-timer timeout nil
                                             (lambda () (finish nil 'timeout))))
                (finish nil 'connection-error))))
        (error (finish nil 'connection-error)))
      (lambda ()
        (unless done (setq done t) (cleanup))))))

(defun sumibi--jev-allowed-p ()
  "Whether ambient Jev work is permitted in the current buffer."
  (and sumibi-mode sumibi-ambient-enable
       (eq sumibi-ambient-backend 'jev)
       (not sumibi-select-mode) (not (region-active-p))
       (not buffer-read-only)
       (not (sumibi-should-exclude-auto-convert-p))))

(defun sumibi--jev-settings ()
  "Return settings whose changes invalidate an observation."
  (list sumibi-jev-threshold sumibi-jev-model sumibi-jev-max-context-length
        sumibi-ambient-punctuation-delay major-mode buffer-file-name))

(defun sumibi--jev-invalidate ()
  "Invalidate pending observations before a command or buffer modification."
  (cl-incf sumibi--jev-generation)
  (setq sumibi--jev-pending nil)
  (when sumibi--jev-timer (cancel-timer sumibi--jev-timer))
  (setq sumibi--jev-timer nil))

(defun sumibi--jev-after-change (&rest _args)
  "Invalidate requests after text changes, including programmatic edits."
  (sumibi--jev-invalidate))

(defun sumibi--jev-pre-command ()
  "Remember the pre-command text revision and invalidate old observations."
  (setq sumibi--jev-pre-command-tick (buffer-chars-modified-tick))
  (sumibi--jev-invalidate))

(defun sumibi--jev-post-command ()
  "Observe deletions and other edits without sending again for self insertion."
  (when (and sumibi--jev-pre-command-tick
             (not (eq this-command 'self-insert-command))
             (not (equal sumibi--jev-observed-generation sumibi--jev-generation))
             (/= sumibi--jev-pre-command-tick (buffer-chars-modified-tick))
             (sumibi--jev-allowed-p))
    (when-let ((snapshot (sumibi--jev-snapshot 0)))
      (plist-put snapshot :key
                 (if (memq this-command '(backward-delete-char-untabify delete-backward-char))
                     "BACKSPACE" "EDIT"))
      (sumibi--jev-submit snapshot))))

(defun sumibi--jev-stop ()
  "Cancel timers and HTTP work on mode changes or buffer destruction."
  (sumibi--jev-invalidate)
  (when sumibi--jev-cancel-request (funcall sumibi--jev-cancel-request))
  (setq sumibi--jev-inflight nil sumibi--jev-cancel-request nil))

(defun sumibi-jev-reset-call-count ()
  "Reset the current buffer's paid Jev request counter after reviewing costs."
  (interactive)
  (setq sumibi--jev-call-count 0 sumibi--jev-last-error nil)
  (message "Sumibi Jev: request counter reset"))

(defun sumibi--jev-notice (error)
  "Report ERROR once per distinct failure without keys, input, or response bodies."
  (unless (eq error sumibi--jev-last-error)
    (sumibi--jev-log "error=%s; input unchanged" error)
    (setq sumibi--jev-last-error error)
    (message "Sumibi Jev: %s; input unchanged (Ctrl+J remains available)" error)))

(defun sumibi--jev-input-context (end)
  "Return (CONTEXT . LOWER-BOUND) at END, respecting syntax delimiters.
Comments and string contents are prose; other programming text is code.
Use the major mode's syntax parser, including syntax properties."
  (save-excursion
    (if (not (derived-mode-p 'prog-mode))
        (cons "plain" (line-beginning-position))
      (let* ((state (syntax-ppss end))
             (string (nth 3 state))
             (comment (nth 4 state))
             (origin (nth 8 state)))
        (if (not (or string comment))
            (cons "code" (line-beginning-position))
          (let ((start (max (line-beginning-position) origin)))
            ;; Find the first position inside the construct, not its opener.
            ;; This handles multi-character comment/string delimiters too.
            (while (and (< start end)
                        (let ((parsed (syntax-ppss start)))
                          (not (and (equal (nth 8 parsed) origin)
                                    (if string (nth 3 parsed) (nth 4 parsed))))))
              (setq start (1+ start)))
            ;; Preserve decorated prefixes such as Lisp's ";;;" as well.
            (when (and comment (>= origin (line-beginning-position))
                       (stringp comment-start-skip))
              (goto-char origin)
              (when (looking-at comment-start-skip)
                (setq start (max start (min end (match-end 0))))))
            (cons "plain" start)))))))

(defun sumibi--jev-snapshot (pause-ms)
  "Capture a bounded input observation after PAUSE-MS milliseconds."
  (when (sumibi--jev-allowed-p)
    (let* ((end (point))
           (context (sumibi--jev-input-context end))
           (start (save-excursion
                    (skip-chars-backward (concat sumibi-skip-chars "。、？")
                                         (cdr context))
                    (point)))
           (text (buffer-substring-no-properties start end)))
      (when (and (<= (length text) sumibi-jev-max-context-length)
                 (string-match-p "[a-zA-Z]" text)
                 (not (text-property-not-all start end 'read-only nil))
                 (not (cl-some (lambda (overlay) (overlay-get overlay 'read-only))
                               (overlays-in start end)))
                 (not (memq (char-before) '(?! ?！))))
        (list :buffer (current-buffer) :generation sumibi--jev-generation
              :tick (buffer-chars-modified-tick) :point end :start start
              :text text :key (char-to-string (char-before)) :pause pause-ms
              :context (car context)
              :settings (sumibi--jev-settings))))))

(defun sumibi--jev-valid-p (snapshot)
  "Test that SNAPSHOT still describes an eligible, unmodified input state."
  (and (eq (current-buffer) (plist-get snapshot :buffer))
       (sumibi--jev-allowed-p)
       (or noninteractive (eq (current-buffer) (window-buffer (selected-window))))
       (= sumibi--jev-generation (plist-get snapshot :generation))
       (= (buffer-chars-modified-tick) (plist-get snapshot :tick))
       (= (point) (plist-get snapshot :point))
       (equal (sumibi--jev-settings) (plist-get snapshot :settings))
       (<= (point-min) (plist-get snapshot :start))
       (equal (buffer-substring-no-properties (plist-get snapshot :start) (point))
              (plist-get snapshot :text))))

(defun sumibi--jev-payload (snapshot)
  "Encode SNAPSHOT without future text or expected labels."
  (encode-coding-string
   (json-encode
    `((state . ((task . "Japanese romaji input method automatic-conversion trigger")
                (buffer_before_cursor . ,(plist-get snapshot :text))
                (latest_key . ,(plist-get snapshot :key))
                (context . ,(plist-get snapshot :context))
                (pause_after_latest_key_ms . ,(plist-get snapshot :pause))))
      (model . ,sumibi-jev-model)
      (questions . ((convert_now . ,sumibi--jev-question)))))
   'utf-8))

(defun sumibi--jev-finish (snapshot)
  "Release SNAPSHOT's request slot and submit the newest queued input."
  (when (eq snapshot sumibi--jev-inflight)
    (setq sumibi--jev-inflight nil sumibi--jev-cancel-request nil)
    (let ((next sumibi--jev-pending))
      (setq sumibi--jev-pending nil)
      (when (and next (sumibi--jev-valid-p next)) (sumibi--jev-submit next)))))

(defun sumibi--jev-submit (snapshot)
  "Submit SNAPSHOT or replace the queued observation when already busy."
  (if sumibi--jev-inflight
      (progn
        (sumibi--jev-log "queued generation=%d (latest input only)"
                         (plist-get snapshot :generation))
        (setq sumibi--jev-pending snapshot))
    (let ((key (getenv "TYPESAFE_API_KEY")))
      (cond
       ((not (and key (not (string-empty-p key)))) (sumibi--jev-notice 'missing-api-key))
       ((not (and (numberp sumibi-jev-threshold) (<= 0 sumibi-jev-threshold 1)
                  (numberp sumibi-jev-timeout) (> sumibi-jev-timeout 0)
                  (integerp sumibi-jev-max-calls-per-buffer)
                  (> sumibi-jev-max-calls-per-buffer 0)))
        (sumibi--jev-notice 'invalid-settings))
       ((>= sumibi--jev-call-count sumibi-jev-max-calls-per-buffer)
        (sumibi--jev-notice 'request-limit))
       (t
        (setq sumibi--jev-inflight snapshot)
        (plist-put snapshot :phase 'decision)
        (cl-incf sumibi--jev-call-count)
        (sumibi--jev-log "request=%d generation=%d chars=%d pause=%dms context=%s"
                         sumibi--jev-call-count (plist-get snapshot :generation)
                         (length (plist-get snapshot :text)) (plist-get snapshot :pause)
                         (plist-get snapshot :context))
        (let ((cancel
               (sumibi--jev-http-post
                sumibi-jev-endpoint
                `(("Authorization" . ,(concat "Bearer " key))
                  ("Content-Type" . "application/json"))
                (sumibi--jev-payload snapshot) sumibi-jev-timeout
                (lambda (body error) (sumibi--jev-decision snapshot body error)))))
          (when (and (eq sumibi--jev-inflight snapshot)
                     (eq (plist-get snapshot :phase) 'decision))
            (setq sumibi--jev-cancel-request cancel))))))))

(defun sumibi--jev-decision (snapshot body failure)
  "Process a Jev decision BODY or FAILURE for SNAPSHOT."
  (when (buffer-live-p (plist-get snapshot :buffer))
    (with-current-buffer (plist-get snapshot :buffer)
      (when (eq snapshot sumibi--jev-inflight)
        (if (not (sumibi--jev-valid-p snapshot))
            (progn
              (sumibi--jev-log "decision discarded: stale generation=%d"
                               (plist-get snapshot :generation))
              (sumibi--jev-finish snapshot))
          (let (score)
            (unless failure
              (condition-case nil
                  (setq score (gethash "noul" (gethash "convert_now"
                                                      (gethash "answers" (json-parse-string body)))))
                (error (setq failure 'invalid-response)))
              (unless (and (numberp score) (<= 0 score 1))
                (setq failure 'invalid-response)))
            (unless failure
              (sumibi--jev-log "score=%.3f threshold=%.3f action=%s generation=%d"
                               score sumibi-jev-threshold
                               (if (>= score sumibi-jev-threshold) "convert" "wait")
                               (plist-get snapshot :generation)))
            (cond
             (failure (sumibi--jev-notice failure) (sumibi--jev-finish snapshot))
             ((>= score sumibi-jev-threshold)
              (setq sumibi--jev-last-error nil)
              (sumibi--jev-convert snapshot))
             (t (setq sumibi--jev-last-error nil) (sumibi--jev-finish snapshot)))))))))

(defun sumibi--jev-convert (snapshot)
  "Request kanji conversion without deleting or blocking the input."
  (plist-put snapshot :phase 'conversion)
  (sumibi--jev-log "conversion started generation=%d" (plist-get snapshot :generation))
  (let* ((raw (plist-get snapshot :text))
         (trimmed (string-trim-right raw))
         (last (and (> (length trimmed) 0) (substring trimmed -1)))
         (mark (and (member last '("." "," "?" "。" "、" "？")) last))
         (suffix (cond ((equal mark ".") "。") ((equal mark ",") "、")
                       ((equal mark "?") "？") (mark mark) (t "")))
         (roman (if mark (substring trimmed 0 -1) trimmed))
         (fixed (assoc roman sumibi-japanese-transliteration-rules))
         (callback
          (lambda (strings error)
            (when (buffer-live-p (plist-get snapshot :buffer))
              (with-current-buffer (plist-get snapshot :buffer)
                (when (eq snapshot sumibi--jev-inflight)
                  (if (sumibi--jev-valid-p snapshot)
                    (if error
                        (sumibi--jev-notice error)
                      (sumibi--jev-apply snapshot roman suffix strings
                                        (if fixed "固定文字列" "LLM")))
                    (sumibi--jev-log "conversion discarded: stale generation=%d"
                                     (plist-get snapshot :generation)))
                  (sumibi--jev-finish snapshot)))))))
    (condition-case nil
        (if (not (sumibi-get-api-key))
            (funcall callback nil 'missing-conversion-api-key)
          (unless sumibi-init
            (sumibi-load-history-from-file)
            (setq sumibi-init t))
          (if fixed
              (funcall callback (list (cadr fixed)) nil)
            (let* ((sumibi-debug nil)
                   (cancel (sumibi-roman-to-kanji-with-surrounding
                            roman roman (sumibi-determine-number-of-n roman)
                            nil callback)))
              (when (eq snapshot sumibi--jev-inflight)
                (setq sumibi--jev-cancel-request cancel)))))
      (error (funcall callback nil 'conversion-error)))))

(defun sumibi--jev-apply (snapshot roman suffix strings &optional source)
  "Atomically apply STRINGS for SNAPSHOT using existing candidates and history."
  (let* ((sumibi-debug nil)
         (original-strings (copy-sequence strings))
         (strings (delete-dups (sumibi-supplement-kouho strings)))
         (candidates
          (cl-loop for str in strings for index from 0
                   collect (list (concat (if (string-empty-p suffix) str
                                           (string-trim-right
                                            str (pcase suffix
                                                  ("。" "[.。]+")
                                                  ("、" "[,、]+")
                                                  (_ "[?？]+"))))
                                         suffix)
                                 (format "%s %d"
                                         (if (member str original-strings)
                                             (or source "LLM") "辞書")
                                         (1+ index))
                                 0 (sumibi-determine-candidate-type str) index)))
         (start (plist-get snapshot :start))
         (end (plist-get snapshot :point)))
    (when candidates
      (sumibi--jev-log "conversion applying generation=%d candidates=%d"
                       (plist-get snapshot :generation) (length candidates))
      (undo-boundary)
      (atomic-change-group
        (setq sumibi-genbun roman sumibi-last-roman (plist-get snapshot :text)
              sumibi-henkan-kouho-list
              (append candidates (list (list (plist-get snapshot :text) "原文まま" 0 'l (length candidates))))
              sumibi-cand-cur 0 sumibi-cand-len (1+ (length candidates)))
        (delete-region start end)
        (goto-char start)
        (insert (sumibi-get-display-string))
        (unless (derived-mode-p 'prog-mode)
          (sumibi--ensure-space-after-heading start))
        (sumibi-display-function start (point) nil)
        (sumibi-select-kakutei))
      (undo-boundary))))

(defun sumibi--jev-post-self-insert ()
  "Observe self insertion; retain punctuation debounce and never block typing."
  (sumibi--jev-invalidate)
  (setq sumibi--jev-observed-generation sumibi--jev-generation)
  (when (sumibi--jev-allowed-p)
    (if (memq (char-before) '(?. ?, ?? ?。 ?、 ?？))
        (let ((buffer (current-buffer))
              (generation sumibi--jev-generation)
              (delay (max 0.5 (or sumibi-ambient-punctuation-delay 0))))
          (setq sumibi--jev-timer
                (run-with-timer
                 delay nil
                 (lambda ()
                   (when (buffer-live-p buffer)
                     (with-current-buffer buffer
                       (when (= generation sumibi--jev-generation)
                         (setq sumibi--jev-timer nil)
                         (when-let ((snapshot (sumibi--jev-snapshot (round (* 1000 delay)))))
                           (sumibi--jev-submit snapshot)))))))))
      (when-let ((snapshot (sumibi--jev-snapshot 0)))
        (sumibi--jev-submit snapshot)))))

(provide 'sumibi-jev)
;;; sumibi-jev.el ends here
