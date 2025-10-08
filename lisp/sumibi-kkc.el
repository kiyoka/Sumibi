;;; sumibi-kkc.el --- KKC integration for Sumibi -*- lexical-binding: t; -*-

;; This file provides KKC (Kana Kanji Converter) integration for Sumibi.
;; Issue #92: Use KKC to reorder Mozc candidates based on context.

(require 'cl-lib)

(defvar sumibi-backend)  ; Defined in sumibi.el
(declare-function sumibi-debug-print "sumibi")

(defcustom sumibi-kkc-command "kkc"
  "Path to KKC command."
  :type 'string
  :group 'sumibi)

(defcustom sumibi-kkc-context-chars 20
  "Number of characters before point to use as context."
  :type 'integer
  :group 'sumibi)

(defun sumibi-kkc--call (input)
  "Call KKC decoder with INPUT string. Returns top candidate or nil."
  (when (executable-find sumibi-kkc-command)
    (condition-case nil
        (let* ((output (with-temp-buffer
                         (call-process-region input nil sumibi-kkc-command
                                              nil t nil "decoder")
                         (buffer-string))))
          (sumibi-debug-print (format "KKC request: %s\n" input))
          (sumibi-debug-print (format "KKC response: %s\n" output))
          (when (string-match "^0: \\(.+\\)" output)
            (let ((segments (match-string 1 output))
                  (result ""))
              (with-temp-buffer
                (insert segments)
                (goto-char (point-min))
                (while (re-search-forward "<\\([^/>]+\\)/[^>]+>" nil t)
                  (setq result (concat result (match-string 1)))))
              (sumibi-debug-print (format "KKC converted result: %s\n" result))
              result)))
      (error nil))))

(defun sumibi-kkc--get-context ()
  "Get Japanese text context before point."
  (save-excursion
    (let* ((end (point))
           (start (max (point-min) (- end sumibi-kkc-context-chars)))
           (text (buffer-substring-no-properties start end)))
      (replace-regexp-in-string "[^ぁ-んァ-ヶー一-龯々]" "" text))))

(defun sumibi-kkc-reorder-candidates (roman candidates)
  "Reorder CANDIDATES using KKC based on context. ROMAN is the reading."
  (if (not (eq sumibi-backend 'mozc-kkc))
      candidates
    (condition-case nil
        (let* ((context (sumibi-kkc--get-context))
               (hiragana (or (cl-find-if
                              (lambda (c)
                                (and (stringp c)
                                     (string-match-p "^[ぁ-ん]+$" c)))
                              candidates)
                             roman))
               (input (concat context hiragana))
               (kkc-result (when (> (length input) 0)
                             (sumibi-kkc--call input))))
          (sumibi-debug-print (format "KKC reorder: roman=%s, context=%s, hiragana=%s\n" roman context hiragana))
          (sumibi-debug-print (format "KKC reorder: original candidates=%s\n" candidates))
          (if (and kkc-result (> (length kkc-result) 0))
              (let* ((context-len (length context))
                     (converted (if (> (length kkc-result) context-len)
                                    (substring kkc-result context-len)
                                  kkc-result))
                     (match (cl-find converted candidates :test #'string=)))
                (sumibi-debug-print (format "KKC reorder: converted=%s, match=%s\n" converted match))
                (if match
                    (let ((reordered (cons match (remove match candidates))))
                      (sumibi-debug-print (format "KKC reorder: reordered candidates=%s\n" reordered))
                      reordered)
                  (progn
                    (sumibi-debug-print "KKC reorder: no match found, keeping original order\n")
                    candidates)))
            (progn
              (sumibi-debug-print "KKC reorder: no KKC result, keeping original order\n")
              candidates)))
      (error candidates))))

(provide 'sumibi-kkc)
;;; sumibi-kkc.el ends here
