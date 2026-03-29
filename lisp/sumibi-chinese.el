;;; sumibi-chinese.el --- Chinese input method powered by LLM API (Pinyin)   -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 Kiyoka Nishiyama
;;
;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Version: 0.1.0
;; Keywords: lisp, ime, chinese, pinyin
;; Package-Requires: ((emacs "29.0") (sumibi "5.4.0"))
;; URL: https://github.com/kiyoka/Sumibi
;;
;; This file is part of Sumibi
;;
;; Sumibi is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Sumibi is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Sumibi; see the file COPYING.

;;; Commentary:

;; 本模块将拼音(pinyin)转换为中文(简体字)。
;; 复用 sumibi.el 的 LLM API 基础设施，作为独立的 minor mode 运行。
;;
;; 使用方法:
;;
;;   (require 'sumibi-chinese)
;;   (global-sumibi-chinese-mode 1)
;;
;; 输入拼音后按 C-j 转换为中文。
;;
;; 例:
;;   wo shi zhongguo ren  →  我是中国人
;;   ni hao ma            →  你好吗
;;

;;; Code:

(require 'sumibi)

;; --------------------------------------------------------------
;; 自定义变量
;; --------------------------------------------------------------

(defgroup sumibi-chinese nil
  "Sumibi 中文(拼音)输入法。"
  :group 'input-method
  :group 'Chinese)

(defvar sumibi-chinese-skip-chars "a-zA-Z0-9:' \t"
  "拼音转换时，作为转换对象的字符集。
包含声调数字(1-4)，因此数字也在范围内。")

;; --------------------------------------------------------------
;; 系统提示词
;; --------------------------------------------------------------

(defconst sumibi-chinese-system-prompt
  (concat
   "你是一个将拼音(pinyin)转换为中文(简体字)的助手。"
   "给定拼音后，请将其转换为适当的中文句子。"
   "如果拼音中包含声调数字(1-4)，请参考它们进行转换。"
   "只输出转换后的句子，不要添加任何注释或说明。"
   "如果输入的是英文句子，请翻译成中文。")
  "中文转换用的系统提示词。")

;; --------------------------------------------------------------
;; 拼音→汉字 转换函数 (调用LLM)
;; --------------------------------------------------------------

(defun sumibi-chinese-pinyin-to-hanzi (pinyin surrounding arg-n deferred-func2)
  "使用LLM API将拼音文本转换为中文。

PINYIN: 要转换的拼音字符串
SURROUNDING: 转换对象周围的文章
ARG-N: 返回多少个候选
DEFERRED-FUNC2: 异步调用时的回调函数(2)。
返回值: (\"第1个句子\" \"第2个句子\" ...)"
  (sumibi-debug-print (format "sumibi-chinese-pinyin-to-hanzi()\n"))
  (sumibi-debug-print (format "  pinyin: %s\n" pinyin))
  (let ((saved-marker (point-marker))
        (result nil))
    (sumibi-openai-http-post
     (list
      ;; 系统提示词
      (cons "system" sumibi-chinese-system-prompt)
      ;; Few-shot 示例 1
      (cons "user"
            (concat
             "请将以下拼音转换为中文。"
             "周围的文章是「大家好！wo shi zhongguo ren，很高兴认识你。」"
             "请参考上下文选择合适的词汇。: wo shi zhongguo ren"))
      (cons "assistant" "我是中国人")
      ;; Few-shot 示例 2
      (cons "user"
            (concat
             "请将以下拼音转换为中文。"
             "周围的文章是「天气预报说jin tian tian qi hen hao可以出去玩。」"
             "请参考上下文选择合适的词汇。: jin tian tian qi hen hao"))
      (cons "assistant" "今天天气很好")
      ;; Few-shot 示例 3
      (cons "user"
            (concat
             "请将以下拼音转换为中文。"
             "周围的文章是「你好！ni hao ma？我很好。」"
             "请参考上下文选择合适的词汇。: ni hao ma"))
      (cons "assistant" "你好吗")
      ;; Few-shot 示例 4
      (cons "user"
            (concat
             "请将以下拼音转换为中文。"
             "周围的文章是「我想xue xi zhong wen，因为我喜欢中国文化。」"
             "请参考上下文选择合适的词汇。: xue xi zhong wen"))
      (cons "assistant" "学习中文")
      ;; Few-shot 示例 5
      (cons "user"
            (concat
             "请将以下拼音转换为中文。"
             "周围的文章是「Please help me translate: zhe shi yi ge hen hao de ji hui」"
             "请参考上下文选择合适的词汇。: zhe shi yi ge hen hao de ji hui"))
      (cons "assistant" "这是一个很好的机会")
      ;; 实际的转换请求
      (cons "user"
            (format
             (concat
              "请将以下拼音转换为中文。"
              "周围的文章是「%s」"
              "请参考上下文选择合适的词汇。: %s")
             surrounding pinyin)))
     arg-n
     ;; 同步回调
     (lambda (json-str)
       (let* ((json-obj (json-parse-string json-str))
              (lst (sumibi-analyze-openai-json-obj json-obj arg-n)))
         (setq result lst)))
     ;; 异步回调
     (lambda (json-str)
       (let* ((json-obj (json-parse-string json-str))
              (lst (sumibi-analyze-openai-json-obj json-obj arg-n)))
         (when (and lst (null deferred-func2))
           (setq result lst))
         (when lst
           (save-excursion
             (goto-char (marker-position saved-marker))
             (insert (car lst))))))
     deferred-func2)
    result))

;; --------------------------------------------------------------
;; 候选数量的决定
;; --------------------------------------------------------------

(defun sumibi-chinese-determine-number-of-n (text)
  "根据 TEXT 的长度决定候选数量。"
  (if (> (length text) sumibi-threshold-letters-of-long-sentence)
      1
    3))

;; --------------------------------------------------------------
;; 拼音转换流水线
;; --------------------------------------------------------------

(defun sumibi-chinese-henkan (pinyin surrounding-text arg-n deferred-func2)
  "将拼音转换为中文。
PINYIN: 拼音字符串
SURROUNDING-TEXT: 周围文本
ARG-N: 候选数量
DEFERRED-FUNC2: 异步回调"
  (let ((lst (sumibi-chinese-pinyin-to-hanzi pinyin surrounding-text arg-n deferred-func2)))
    (append
     (-map
      (lambda (x)
        (list (car x)
              (sumibi--annotation-label (car x) (+ 1 (cdr x)))
              0
              'j
              (cdr x)))
      (-zip-pair
       lst
       '(0 1 2 3 4 5 6 7 8 9
         10 11 12 13 14 15 16 17 18 19)))
     (list
      (list pinyin "原文" 0 'l (length lst))))))

;; --------------------------------------------------------------
;; 转换请求
;; --------------------------------------------------------------

(defun sumibi-chinese-henkan-request (pinyin surrounding-text deferred-func2)
  "发送拼音转换请求。
PINYIN: 拼音字符串
SURROUNDING-TEXT: 周围文本
DEFERRED-FUNC2: 异步回调"
  (sumibi-chinese-henkan pinyin surrounding-text
                         (sumibi-chinese-determine-number-of-n pinyin)
                         deferred-func2))

;; --------------------------------------------------------------
;; 区域转换 (同步)
;; --------------------------------------------------------------

(defun sumibi-chinese-henkan-region-sync (b e)
  "将区域从拼音转换为中文(同步版)。
B: 区域的起始位置
E: 区域的结束位置"
  (when (/= b e)
    (let* ((yomi (buffer-substring-no-properties b e))
           (surrounding-text (sumibi-extract-lines-around-point b (ceiling (/ (max 2 sumibi-surrounding-lines) 2))))
           (henkan-list (sumibi-chinese-henkan-request yomi surrounding-text nil)))
      (setq sumibi-genbun yomi)
      (if henkan-list
          (condition-case err
              (progn
                (setq
                 sumibi-henkan-kouho-list henkan-list
                 sumibi-cand-cur 0
                 sumibi-cand-len (length henkan-list))

                (sumibi-debug-print (format "chinese henkan-kouho-list:%s \n" sumibi-henkan-kouho-list))

                (setq sumibi-last-roman (buffer-substring-no-properties b e))
                (delete-region b e)
                (goto-char b)
                (insert (sumibi-get-display-string))
                (setq e (point))
                (sumibi-display-function b e nil)
                (sumibi-select-kakutei)
                t)
            (sumibi-trap-server-down
             (beep)
             (message (error-message-string err))
             (setq sumibi-select-mode nil))
            (run-hooks 'sumibi-select-mode-end-hook))
        nil))))

;; --------------------------------------------------------------
;; 区域转换 (异步)
;; --------------------------------------------------------------

(defun sumibi-chinese-henkan-region-async (b e)
  "将区域从拼音转换为中文(异步版)。
B: 区域的起始位置
E: 区域的结束位置"
  (when (/= b e)
    (let ((yomi (buffer-substring-no-properties b e))
          (saved-b-marker 0)
          (saved-e-marker 0)
          (cur-buf (current-buffer)))
      (setq sumibi-genbun yomi)
      (deactivate-mark)
      (goto-char e)
      (setq saved-e-marker (point-marker))
      (goto-char b)
      (setq saved-b-marker (point-marker))
      (goto-char e)
      (let ((yomi-overlay (make-overlay b e)))
        (overlay-put yomi-overlay 'display yomi)
        (overlay-put yomi-overlay 'face '(:foreground "gray"))
        (sumibi-chinese-henkan-request
         yomi
         yomi
         (lambda ()
           (with-current-buffer cur-buf
             (save-excursion
               (delete-overlay yomi-overlay)
               (delete-region (marker-position saved-b-marker)
                              (marker-position saved-e-marker))))))))))

;; --------------------------------------------------------------
;; 区域转换 (分发)
;; --------------------------------------------------------------

(defun sumibi-chinese-henkan-region (b e)
  "将指定区域转换为中文。根据字符数决定同步/异步。
B: 区域的起始位置
E: 区域的结束位置"
  (sumibi-init)
  (when sumibi-init
    (when (/= b e)
      (if (sumibi-determine-sync-p (buffer-substring-no-properties b e))
          (sumibi-chinese-henkan-region-sync b e)
        (sumibi-chinese-henkan-region-async b e)))))

;; --------------------------------------------------------------
;; 拼音向后跳过
;; --------------------------------------------------------------

(defun sumibi-chinese-skip-chars-backward ()
  "拼音转换时，向后跳过转换对象的拼音字符。"
  (let* ((skip-chars
          (if auto-fill-function
              (concat sumibi-chinese-skip-chars "\n")
            sumibi-chinese-skip-chars))
         (pos (or (and (markerp (mark-marker)) (marker-position (mark-marker)))
                  1))
         (result (save-excursion
                   (skip-chars-backward skip-chars (and (< pos (point)) pos)))))
    result))

;; --------------------------------------------------------------
;; 转换触发器 (C-j)
;; --------------------------------------------------------------

(defun sumibi-chinese-trans ()
  "执行拼音→中文转换。
从光标位置向行首方向，在拼音连续的范围内进行转换。"
  (interactive)
  (sumibi-debug-print "sumibi-chinese-trans()")

  (cond
   ;; 选定了区域的情况
   ((region-active-p)
    (let ((b (region-beginning))
          (e (region-end)))
      (sumibi-chinese-henkan-region b e)))

   ;; 未选定区域的情况
   (t
    (cond
     (sumibi-select-mode
      (sumibi-debug-print "<<sumibi-select-mode (chinese)>>\n")
      (funcall (lookup-key sumibi-select-mode-map sumibi-rK-trans-key)))

     (t
      (cond
       ((eq (sumibi-char-charset (preceding-char)) 'ascii)
        (sumibi-debug-print (format "chinese: ascii? (%s) => t\n" (preceding-char)))
        (let ((end (point))
              (gap (sumibi-chinese-skip-chars-backward)))
          (when (/= gap 0)
            (let ((b (+ end gap))
                  (e end))
              (sumibi-chinese-henkan-region b e)))))

       (t
        (sumibi-debug-print (format "chinese: <<OTHER>> (%s)\n" (preceding-char))))))))))

;; --------------------------------------------------------------
;; Minor mode 定义
;; --------------------------------------------------------------

(defvar sumibi-chinese-mode nil
  "非nil时表示 Sumibi Chinese mode 已启用。")
(make-variable-buffer-local 'sumibi-chinese-mode)

(defvar sumibi-chinese-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'sumibi-chinese-trans)
    map)
  "拼音→中文转换键映射。")

(add-to-list 'minor-mode-alist '(sumibi-chinese-mode " [中]"))
(add-to-list 'minor-mode-map-alist (cons 'sumibi-chinese-mode sumibi-chinese-mode-map))

(defun sumibi-chinese-mode (&optional arg)
  "将拼音转换为中文(简体字)的 minor mode。
指定正数参数ARG时启用。

\\<sumibi-chinese-mode-map>\\[sumibi-chinese-trans] 将拼音转换为中文。

例:
  wo shi zhongguo ren  →  我是中国人
  ni hao ma            →  你好吗"
  (interactive "P")
  (setq sumibi-chinese-mode
        (if (null arg)
            (not sumibi-chinese-mode)
          (> (prefix-numeric-value arg) 0)))
  (force-mode-line-update))

(defun global-sumibi-chinese-mode (&optional arg)
  "在所有缓冲区中启用拼音→中文转换模式。
指定正数参数ARG时启用。"
  (interactive "P")
  (sumibi-chinese-mode-internal arg t))

(defun sumibi-chinese-mode-internal (arg global)
  "变更 Sumibi Chinese mode 的通用函数。
参数ARG: 未使用
参数GLOBAL: 是否在所有缓冲区中启用"
  (or (local-variable-p 'sumibi-chinese-mode (current-buffer))
      (make-local-variable 'sumibi-chinese-mode))
  (if global
      (progn
        (setq-default sumibi-chinese-mode
                      (if (null arg) (not sumibi-chinese-mode)
                        (> (prefix-numeric-value arg) 0)))
        ;; 清除所有缓冲区的 buffer-local 值
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (kill-local-variable 'sumibi-chinese-mode))))
    (setq sumibi-chinese-mode
          (if (null arg) (not sumibi-chinese-mode)
            (> (prefix-numeric-value arg) 0))))
  (force-mode-line-update))

(provide 'sumibi-chinese)

;;; sumibi-chinese.el ends here
