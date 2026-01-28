;;; sumibi-romaji-ratio-test.el --- Tests for romaji ratio functions -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: tests

;;; Commentary:
;; Test cases for sumibi-romaji-ratio and related functions.

;;; Code:

(require 'ert)

;; Provide minimal stubs for dependencies if not available
(unless (require 'popup nil 'noerror)
  (defvar popup-version "0-stub")
  (defun popup-menu* (&rest _args) (error "popup stub: not implemented"))
  (provide 'popup))

(unless (require 'unicode-escape nil 'noerror)
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
      (dolist (item list)
        (when (funcall fn item)
          (push item result)))
      (nreverse result)))
  (defun -map (fn list)
    (mapcar fn list))
  (provide 'dash))

;; Add lisp directory to load-path
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))

(require 'sumibi)

;;; テスト: sumibi-romaji-converted-chars

(ert-deftest test-romaji-converted-chars-basic ()
  "Basic romaji should return converted character count."
  ;; "watashi" (7文字) → "わたし" (3文字) → 3*2=6文字
  (should (= (sumibi-romaji-converted-chars "watashi") 6))
  ;; "nihon" (5文字) → "にほん" (3文字) → 3*2=5文字(元の文字数を超えないため5)
  (should (= (sumibi-romaji-converted-chars "nihon") 5))
  ;; "kyou" (4文字) → "きょう" (3文字) → 3*2=4文字(元の文字数を超えないため4)
  (should (= (sumibi-romaji-converted-chars "kyou") 4)))

(ert-deftest test-romaji-converted-chars-english ()
  "English words should return 0 (not converted)."
  ;; 英単語は変換されない（sumibi-romaji-to-hiraganaで判定）
  (should (= (sumibi-romaji-converted-chars "hello") 0))
  (should (= (sumibi-romaji-converted-chars "world") 0)))

(ert-deftest test-romaji-converted-chars-mixed ()
  "Mixed characters are partially converted."
  ;; "test123" → "てst123" (teがてに変換) → ひらがな1文字 → 2
  (should (> (sumibi-romaji-converted-chars "test123") 0))
  ;; "abc@def" → 数字・記号は変換されないが、"a"は"あ"に変換可能
  (should (> (sumibi-romaji-converted-chars "abc@def") 0)))

;;; テスト: sumibi-romaji-ratio

(ert-deftest test-romaji-ratio-empty ()
  "Empty string should return 0.0."
  (should (= (sumibi-romaji-ratio "") 0.0))
  (should (= (sumibi-romaji-ratio nil) 0.0)))

(ert-deftest test-romaji-ratio-pure-romaji ()
  "Pure romaji text should have high ratio."
  ;; "watashi nihon" = watashi(7→6) + nihon(5→5) = 11/12 ≈ 0.92
  (let ((ratio (sumibi-romaji-ratio "watashi nihon")))
    (should (> ratio 0.8))))

(ert-deftest test-romaji-ratio-pure-english ()
  "Pure English text should have low ratio."
  ;; 英単語は変換されないので、ローマ字比率は0
  (let ((ratio (sumibi-romaji-ratio "hello world")))
    (should (< ratio 0.1))))

(ert-deftest test-romaji-ratio-mixed ()
  "Mixed text should have intermediate ratio."
  ;; "watashi hello" = watashi(7→6) + hello(5→0) = 6/12 = 0.5
  (let ((ratio (sumibi-romaji-ratio "watashi hello")))
    (should (>= ratio 0.4))
    (should (<= ratio 0.6))))

(ert-deftest test-romaji-ratio-english-heavy ()
  "English-heavy text should have low ratio."
  ;; "I will not create watashi"
  ;; I(1→0) + will(4→0) + not(3→0) + create(6→0) + watashi(7→6)
  ;; = 6/21 ≈ 0.29
  (let ((ratio (sumibi-romaji-ratio "I will not create watashi")))
    (should (< ratio 0.5))))

;;; テスト: sumibi-has-sufficient-romaji-p

(ert-deftest test-has-sufficient-romaji-pure-romaji ()
  "Pure romaji text should pass threshold."
  (should (sumibi-has-sufficient-romaji-p "watashi nihon desu")))

(ert-deftest test-has-sufficient-romaji-pure-english ()
  "Pure English text should not pass threshold."
  (should-not (sumibi-has-sufficient-romaji-p "Hello world")))

(ert-deftest test-has-sufficient-romaji-threshold-boundary ()
  "Text near threshold boundary should be handled correctly."
  ;; "watashi hello" は約50%なので、デフォルト閾値50%を境界
  (should (sumibi-has-sufficient-romaji-p "watashi hello"))
  ;; "nihon hello world" = nihon(5→5) + hello(5→0) + world(5→0) = 5/15 ≈ 0.33
  (should-not (sumibi-has-sufficient-romaji-p "nihon hello world")))

(provide 'sumibi-romaji-ratio-test)
;;; sumibi-romaji-ratio-test.el ends here
