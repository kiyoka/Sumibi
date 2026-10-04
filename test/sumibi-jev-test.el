;;; sumibi-jev-test.el --- Jev ambient regression tests -*- lexical-binding: t; -*-
;;; Commentary:
;; No credentials, network, or paid requests are used by these tests.
;;; Code:
(require 'ert)
(unless (require 'popup nil t) (provide 'popup))
(unless (require 'unicode-escape nil t)
  (defun unicode-escape (s) s)
  (defun unicode-escape-to-string (s) s)
  (provide 'unicode-escape))
(unless (require 'deferred nil t) (provide 'deferred))
(unless (require 'dash nil t)
  (defun -filter (fn items) (cl-remove-if-not fn items))
  (defun -map (fn items) (mapcar fn items))
  (provide 'dash))
(require 'sumibi)

(defmacro sumibi-test--jev-buffer (&rest body)
  "Run BODY with fake async transport, conversion and isolated Sumibi state."
  (declare (indent 0))
  `(let ((sumibi-ambient-enable t) (sumibi-mode t)
         (sumibi-ambient-backend 'jev) (sumibi-select-mode nil)
         (sumibi-ambient-punctuation-delay 0.5)
         (sumibi-history-stack nil) (sumibi-use-fence nil)
         (sumibi-debug nil) (sumibi-henkan-kouho-list nil)
         (sumibi-init t)
         (sumibi-jev-max-calls-per-buffer 1000)
         (process-environment (copy-sequence process-environment))
         requests conversions)
     (setenv "TYPESAFE_API_KEY" "test-key")
     (cl-letf (((symbol-function 'sumibi--jev-http-post)
                (lambda (url headers body timeout callback)
                  (push (list url headers body timeout callback) requests)
                  #'ignore))
               ((symbol-function 'sumibi-get-api-key) (lambda () "test-conversion-key"))
               ((symbol-function 'sumibi-roman-to-kanji-with-surrounding)
                (lambda (roman surrounding n deferred &optional callback)
                  (push (list roman surrounding n deferred callback) conversions)
                  #'ignore)))
       (with-temp-buffer
         (unwind-protect (progn ,@body) (sumibi--jev-stop))))))

(defun sumibi-test--jev-answer (request score)
  "Complete fake REQUEST with SCORE."
  (funcall (nth 4 request)
           (json-encode `((answers . ((convert_now . ((noul . ,score))))))) nil))

(ert-deftest sumibi-jev-default-remains-rules ()
  (should (eq (default-value 'sumibi-ambient-backend) 'rules)))

(ert-deftest sumibi-jev-input-payload-and-negative-decision ()
  (sumibi-test--jev-buffer
    (insert "I will review the report ")
    (cl-letf (((symbol-function 'sumibi-is-english-text-p) (lambda (_) (ert-fail "legacy guard used")))
              ((symbol-function 'sumibi-has-sufficient-romaji-p) (lambda (_) (ert-fail "legacy guard used"))))
      (sumibi-check-particle-trigger))
    (let* ((request (car requests))
           (payload (json-parse-string (decode-coding-string (nth 2 request) 'utf-8)))
           (state (gethash "state" payload)))
      (should (equal (car request) sumibi-jev-endpoint))
      (should (equal (gethash "model" payload) "jev-latest"))
      (should (equal (gethash "buffer_before_cursor" state) (buffer-string)))
      (should (equal (gethash "latest_key" state) " "))
      (should (= (gethash "pause_after_latest_key_ms" state) 0))
      (should-not (gethash "gold" state))
      (sumibi-test--jev-answer request 0.45)
      (should-not conversions)
      (should-not sumibi--jev-inflight)
      (should (equal (buffer-string) "I will review the report ")))))

(ert-deftest sumibi-jev-threshold-and-asynchronous-conversion ()
  (sumibi-test--jev-buffer
    (buffer-enable-undo)
    (insert "arigatou gozaimasu ")
    (sumibi-check-particle-trigger)
    (sumibi-test--jev-answer (car requests) 0.8)
    (should (equal (buffer-string) "arigatou gozaimasu "))
    (should (equal (caar conversions) "arigatou gozaimasu"))
    (funcall (nth 4 (car conversions)) '("ありがとうございます") nil)
    (should (equal (buffer-string) "ありがとうございます"))
    (should (= 1 (length sumibi-history-stack)))
    (should (equal (caar sumibi-henkan-kouho-list) "ありがとうございます"))
    (should (equal (car (car (last sumibi-henkan-kouho-list))) "arigatou gozaimasu "))
    (should-not sumibi--jev-inflight)
    (undo-only 1)
    (should (equal (buffer-string) "arigatou gozaimasu "))))

(ert-deftest sumibi-jev-below-threshold-preserves-input ()
  (sumibi-test--jev-buffer
    (insert "ohayou gozaimasu ")
    (sumibi-check-particle-trigger)
    (sumibi-test--jev-answer (car requests) 0.79)
    (should-not conversions)
    (should (equal (buffer-string) "ohayou gozaimasu "))))

(ert-deftest sumibi-jev-stale-decision-coalesces-latest-state ()
  (sumibi-test--jev-buffer
    (sumibi-setup-auto-convert-hook)
    (insert "arigatou ")
    (sumibi-check-particle-trigger)
    (let ((old (car requests)))
      (insert "g") (sumibi-check-particle-trigger)
      (insert "o") (sumibi-check-particle-trigger)
      (should (= 1 (length requests)))
      (sumibi-test--jev-answer old 0.9)
      (should-not conversions)
      (should (= 2 (length requests)))
      (should (equal (plist-get sumibi--jev-inflight :text) "arigatou go")))))

(ert-deftest sumibi-jev-stale-conversion-does-not-edit ()
  (sumibi-test--jev-buffer
    (sumibi-setup-auto-convert-hook)
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (sumibi-test--jev-answer (car requests) 0.9)
    (let ((callback (nth 4 (car conversions))))
      (insert "gozaimasu ") (sumibi-check-particle-trigger)
      (funcall callback '("ありがとう") nil)
      (should (equal (buffer-string) "arigatou gozaimasu "))
      (should-not sumibi-history-stack))))

(ert-deftest sumibi-jev-cursor-movement-invalidates-decision ()
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (sumibi--ambient-pre-command-cancel)
    (backward-char) (forward-char)
    (sumibi-test--jev-answer (car requests) 0.9)
    (should-not conversions)))

(ert-deftest sumibi-jev-programmatic-edits-invalidate-decision ()
  (sumibi-test--jev-buffer
    (sumibi-setup-auto-convert-hook)
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (save-excursion (goto-char (point-min)) (insert "x") (delete-char -1))
    (sumibi-test--jev-answer (car requests) 0.9)
    (should-not conversions)))

(ert-deftest sumibi-jev-disabled-or-selecting-ignores-response ()
  (dolist (setting '(disabled selecting backend))
    (sumibi-test--jev-buffer
      (insert "arigatou ") (sumibi-check-particle-trigger)
      (pcase setting
        ('disabled (setq sumibi-ambient-enable nil))
        ('selecting (setq sumibi-select-mode t))
        ('backend (setq sumibi-ambient-backend 'rules)))
      (sumibi-test--jev-answer (car requests) 0.9)
      (should-not conversions))))

(ert-deftest sumibi-jev-settings-change-invalidates-response ()
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (let ((sumibi-jev-threshold 0.9))
      (sumibi-test--jev-answer (car requests) 0.95)
      (should-not conversions))))

(ert-deftest sumibi-jev-excluded-buffers-never-send ()
  (dolist (context '(shell gpg minibuffer readonly selecting region disabled))
    (sumibi-test--jev-buffer
      (insert "watashiwa ")
      (pcase context
        ('shell (setq major-mode 'shell-mode))
        ('gpg (setq buffer-file-name "/tmp/jev-test.gpg"))
        ('readonly (setq buffer-read-only t))
        ('selecting (setq sumibi-select-mode t))
        ('region (set-mark (point-min)) (setq transient-mark-mode t mark-active t))
        ('disabled (setq sumibi-ambient-enable nil)))
      (cl-letf (((symbol-function 'minibufferp) (lambda (&optional _) (eq context 'minibuffer))))
        (sumibi-check-particle-trigger))
      (should-not requests))))

(ert-deftest sumibi-jev-number-and-exclamation-do-not-send ()
  (dolist (text '("3.14" "123 " "yatta!" "yatta！"))
    (sumibi-test--jev-buffer
      (insert text) (sumibi-check-particle-trigger)
      (should-not requests))))

(ert-deftest sumibi-jev-context-limit-skips-without-truncating ()
  (sumibi-test--jev-buffer
    (let ((sumibi-jev-max-context-length 5))
      (insert "arigatou ") (sumibi-check-particle-trigger)
      (should-not requests))))

(ert-deftest sumibi-jev-key-and-call-limit-preserve-manual-conversion ()
  (sumibi-test--jev-buffer
    (insert "arigatou ")
    (setenv "TYPESAFE_API_KEY" nil)
    (sumibi-check-particle-trigger)
    (should-not requests)
    (setenv "TYPESAFE_API_KEY" "test-key")
    (setq sumibi--jev-call-count 1000)
    (sumibi-check-particle-trigger)
    (should-not requests)
    (sumibi-jev-reset-call-count)
    (sumibi-check-particle-trigger)
    (should (= 1 (length requests)))))

(ert-deftest sumibi-jev-malformed-and-http-errors-preserve-input ()
  (dolist (body '("{}" "garbage" "{\"answers\":{\"convert_now\":{\"noul\":2}}}"))
    (sumibi-test--jev-buffer
      (insert "arigatou ") (sumibi-check-particle-trigger)
      (funcall (nth 4 (car requests)) body nil)
      (should-not conversions)
      (should-not sumibi--jev-inflight)
      (should (equal (buffer-string) "arigatou "))))
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (funcall (nth 4 (car requests)) nil 'http-error)
    (should-not conversions)
    (should-not sumibi--jev-inflight)))

(ert-deftest sumibi-jev-conversion-failure-preserves-space ()
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (sumibi-test--jev-answer (car requests) 0.9)
    (funcall (nth 4 (car conversions)) nil 'timeout)
    (should (equal (buffer-string) "arigatou "))
    (should-not sumibi--jev-inflight)))

(ert-deftest sumibi-jev-punctuation-debounce-and-conversion ()
  (sumibi-test--jev-buffer
    (insert "ashitamo ikimasuka?")
    (sumibi-check-particle-trigger)
    (should-not requests)
    (let ((timer sumibi--jev-timer))
      (should (timerp timer))
      (cancel-timer timer)
      (apply (timer--function timer) (timer--args timer)))
    (should (= (plist-get sumibi--jev-inflight :pause) 500))
    (sumibi-test--jev-answer (car requests) 0.9)
    (should (equal (caar conversions) "ashitamo ikimasuka"))
    (funcall (nth 4 (car conversions)) '("明日も行きますか") nil)
    (should (equal (buffer-string) "明日も行きますか？"))))

(ert-deftest sumibi-jev-next-key-cancels-punctuation-observation ()
  (sumibi-test--jev-buffer
    (insert "hai,") (sumibi-check-particle-trigger)
    (sumibi--ambient-pre-command-cancel)
    (should-not sumibi--jev-timer)
    (should-not requests)))

(ert-deftest sumibi-jev-deletion-observes-new-state ()
  (sumibi-test--jev-buffer
    (sumibi-setup-auto-convert-hook)
    (insert "arigatou x")
    (sumibi--ambient-pre-command-cancel)
    (delete-char -1)
    (let ((this-command 'delete-backward-char))
      (sumibi--jev-post-command))
    (should (= 1 (length requests)))
    (should (equal (plist-get sumibi--jev-inflight :key) "BACKSPACE"))
    (should (equal (plist-get sumibi--jev-inflight :text) "arigatou "))))

(ert-deftest sumibi-jev-wrapped-self-insert-does-not-submit-twice ()
  (sumibi-test--jev-buffer
    (sumibi-setup-auto-convert-hook)
    (sumibi--ambient-pre-command-cancel)
    (insert "a")
    (sumibi-check-particle-trigger)
    (let ((this-command 'org-self-insert-command))
      (sumibi--jev-post-command))
    (should (= 1 (length requests)))
    (should-not sumibi--jev-pending)))

(ert-deftest sumibi-jev-punctuation-minimum-delay-matches-v2 ()
  (sumibi-test--jev-buffer
    (let ((sumibi-ambient-punctuation-delay 0))
      (insert "hai,") (sumibi-check-particle-trigger)
      (should-not requests)
      (let ((timer sumibi--jev-timer))
        (cancel-timer timer)
        (apply (timer--function timer) (timer--args timer)))
      (should (= (plist-get sumibi--jev-inflight :pause) 500)))))

(ert-deftest sumibi-jev-no-duplicate-punctuation-from-converter ()
  (sumibi-test--jev-buffer
    (insert "ashitamo ikimasuka?")
    (let ((snapshot (sumibi--jev-snapshot 500)))
      (setq sumibi--jev-inflight snapshot)
      (sumibi--jev-convert snapshot)
      (funcall (nth 4 (car conversions)) '("明日も行きますか？") nil)
      (should (equal (buffer-string) "明日も行きますか？")))))

(ert-deftest sumibi-jev-readonly-text-never-sends ()
  (sumibi-test--jev-buffer
    (insert (propertize "arigatou " 'read-only t))
    (sumibi-check-particle-trigger)
    (should-not requests))
  (sumibi-test--jev-buffer
    (insert "arigatou ")
    (overlay-put (make-overlay (point-min) (point-max)) 'read-only t)
    (sumibi-check-particle-trigger)
    (should-not requests)))

(ert-deftest sumibi-jev-nonselected-buffer-response-is-stale ()
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (let ((noninteractive nil))
      (sumibi-test--jev-answer (car requests) 0.9)
      (should-not conversions))))

(ert-deftest sumibi-jev-http-cancel-before-response-is-silent ()
  (let ((response (generate-new-buffer " *jev-cancel-test*")) callback results)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url cb &rest _args) (setq callback cb) response)))
      (let ((cancel (sumibi--jev-http-post "https://example.invalid" nil "{}" 1
                                          (lambda (&rest result) (push result results)))))
        (funcall cancel)
        (should-not (buffer-live-p response))
        (with-temp-buffer (funcall callback '(:error cancelled)))
        (should-not results)))))

(ert-deftest sumibi-jev-mode-disable-cancels-work-and-hooks ()
  (sumibi-test--jev-buffer
    (let ((cancelled nil))
      (setq sumibi--jev-cancel-request (lambda () (setq cancelled t)))
      (sumibi-setup-auto-convert-hook)
      (should (memq #'sumibi--jev-after-change after-change-functions))
      (sumibi-mode -1)
      (should cancelled)
      (should-not (memq #'sumibi--jev-after-change after-change-functions)))))

(ert-deftest sumibi-jev-initializes-persisted-history-once ()
  (sumibi-test--jev-buffer
    (let ((sumibi-init nil) (loads 0))
      (cl-letf (((symbol-function 'sumibi-load-history-from-file)
                 (lambda () (cl-incf loads))))
        (insert "arigatou ") (sumibi-check-particle-trigger)
        (sumibi-test--jev-answer (car requests) 0.9)
        (should sumibi-init)
        (should (= loads 1))))))

(ert-deftest sumibi-jev-ctrl-j-restores-candidate-selection ()
  (sumibi-test--jev-buffer
    (insert "arigatou gozaimasu ") (sumibi-check-particle-trigger)
    (sumibi-test--jev-answer (car requests) 0.9)
    (funcall (nth 4 (car conversions)) '("ありがとうございます" "有難うございます") nil)
    (sumibi-rK-trans)
    (should sumibi-select-mode)
    (should (= 3 sumibi-cand-len))
    (sumibi-select-cancel)
    (should (equal (buffer-string) "ありがとうございます"))))

(ert-deftest sumibi-jev-killed-buffer-response-is-ignored ()
  (sumibi-test--jev-buffer
    (insert "arigatou ") (sumibi-check-particle-trigger)
    (let ((buffer (current-buffer)) (request (car requests)))
      (kill-buffer buffer)
      (sumibi-test--jev-answer request 0.9)
      (should-not conversions))))

(ert-deftest sumibi-jev-http-timeout-completes-once-and-cleans-up ()
  (let ((response (generate-new-buffer " *jev-http-test*"))
        timer-function http-callback results)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url callback &rest _args)
                     (setq http-callback callback) response))
                  ((symbol-function 'run-with-timer)
                   (lambda (_seconds _repeat callback &rest _args)
                     (setq timer-function callback) nil)))
          (sumibi--jev-http-post "https://example.invalid" nil "{}" 1
                                 (lambda (body error) (push (list body error) results)))
          (funcall timer-function)
          (should (equal results '((nil timeout))))
          (should-not (buffer-live-p response))
          (with-temp-buffer (funcall http-callback '(:error connection-failed)))
          (should (= 1 (length results))))
      (when (buffer-live-p response) (kill-buffer response)))))

(ert-deftest sumibi-jev-http-success-returns-body-and-cancellation-is-silent ()
  (let ((response (generate-new-buffer " *jev-http-test*")) callback results)
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url cb &rest _args) (setq callback cb) response)))
          (let ((cancel (sumibi--jev-http-post
                         "https://example.invalid" nil "{}" 1
                         (lambda (body error) (push (list body error) results)))))
            (with-current-buffer response
              (setq-local url-http-response-status 200)
              (insert "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"ok\":true}")
              (funcall callback nil))
            (should (equal results '(("{\"ok\":true}" nil))))
            (funcall cancel)
            (should (= 1 (length results)))
            (should-not (buffer-live-p response))))
      (when (buffer-live-p response) (kill-buffer response)))))

(ert-deftest sumibi-jev-existing-conversion-prompt-safe-async-path ()
  (let (handler result)
    (cl-letf (((symbol-function 'sumibi--jev-http-post)
               (lambda (_url _headers _body _timeout cb) (setq handler cb) #'ignore))
              ((symbol-function 'sumibi-get-api-key) (lambda () "test-key")))
      (with-temp-buffer
        (insert "arigatou gozaimasu")
        (let ((cancel (sumibi-roman-to-kanji-with-surrounding
                       (buffer-string) (buffer-string) 1 nil
                       (lambda (strings error) (setq result (list strings error))))))
          (should (functionp cancel))
          (funcall handler "{\"choices\":[{\"message\":{\"content\":\"ありがとうございます\"}}]}" nil)
          (should (equal result '(("ありがとうございます") nil)))
          (should (equal (buffer-string) "arigatou gozaimasu")))))))

(provide 'sumibi-jev-test)
;;; sumibi-jev-test.el ends here
