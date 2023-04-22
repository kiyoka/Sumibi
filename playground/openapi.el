(require 'cl)
(require 'url-parse)

(defun openai-debug-print (string)
  (let
      ((buffer (get-buffer-create "*openai-debug*")))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert string))))

(defun openai-url-http-post (system-content1
			     user-content1
			     assistant-content1
			     user-content2)
  "call OpenAI completions API."
  (progn
    (setq url "https://api.openai.com/v1/chat/completions")
    (setq url-request-method "POST")
    (setq url-http-version "1.1")
    (setq url-request-extra-headers
	  `(("Content-Type" . "application/json")
	    ("Authorization" . ,(concat "Bearer " (getenv "OPENAI_API_KEY")))))
    (setq url-request-data
	  (concat
	   "{"
	   "  \"model\": \"gpt-3.5-turbo\","
	   "  \"temperature\": 0.8,"
	   "  \"messages\": [ "
	   (format " {\"role\": \"system\",    \"content\": \"%s\"}," (url-hexify-string system-content1))
	   (format " {\"role\": \"user\",      \"content\": \"%s\"}," (url-hexify-string user-content1))
	   (format " {\"role\": \"assistant\", \"content\": \"%s\"}," (url-hexify-string assistant-content1))
	   (format " {\"role\": \"user\",      \"content\": \"%s\"} " (url-hexify-string user-content2))
	   "  ] "
	   "}"))
    (let* ((lines
	    (let ((buf (url-retrieve-synchronously url)))
	      (openai-debug-print (buffer-name buf))
	      (openai-debug-print "\n")
	      (if buf
                  (with-current-buffer buf
                    (decode-coding-string 
                     (let ((str (buffer-substring-no-properties (point-min) (point-max))))
                       (cond
                        (url-http-response-status
                         (openai-debug-print (format "http result code:%s\n" url-http-response-status))
                         (openai-debug-print (format "(%d-%d) eoh=%s\n" (point-min) (point-max) url-http-end-of-headers))
                         (openai-debug-print (format "<<<%s>>>\n" str))
                         str)
                        (t
                         "<<TIMEOUT>>\n"
                         )))
                     'utf-8))
                "<<CONNECTION ERROR>>\n")))
           (line-list
            (split-string lines "\n")))
      (cadr (reverse line-list)))))

(defun api-test ()
  (let* ((json-str (openai-url-http-post
		    "あなたはローマ字を日本語に変換するアシスタントです。"
		    "ローマ字の文を漢字仮名混じり文にしてください。 : watashi no namae ha nakano desu ."
		    "私の名前は中野です。"
		    "ローマ字の文を漢字仮名混じり文にしてください。 : nihongo wo henkan surukotoga dekimasu ."))
	 (json-obj (json-parse-string json-str))
	 (hex-str
	  (gethash "content"
		   (gethash "message"
			    (aref (gethash "choices" json-obj) 0))))
	 (utf8-str
	  (decode-coding-string (url-unhex-string hex-str) 'utf-8)))
    utf8-str))

(when t
  (api-test))
