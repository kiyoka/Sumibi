;;; sumibi-english-detection-test.el --- Tests for English text detection  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; This file is part of Sumibi.

;;; Commentary:
;; Tests for English text detection functionality in auto-conversion

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

(unless (require 'sumibi-localdic nil 'noerror)
  (defvar sumibi-localdic-data nil)
  (defun sumibi-localdic-get-candidates (_key) nil)
  (provide 'sumibi-localdic))

(require 'sumibi)

;;; Tests for sumibi--is-english-word

(ert-deftest test-is-english-word-common-words ()
  "Test that common English words are recognized."
  (should (sumibi--is-english-word "will"))
  (should (sumibi--is-english-word "not"))
  (should (sumibi--is-english-word "create"))
  (should (sumibi--is-english-word "pull"))
  (should (sumibi--is-english-word "request"))
  (should (sumibi--is-english-word "the"))
  (should (sumibi--is-english-word "and"))
  (should (sumibi--is-english-word "for")))

(ert-deftest test-is-english-word-case-insensitive ()
  "Test that English word detection is case-insensitive."
  (should (sumibi--is-english-word "Will"))
  (should (sumibi--is-english-word "WILL"))
  (should (sumibi--is-english-word "WiLl")))

(ert-deftest test-is-english-word-non-words ()
  "Test that non-English words return nil."
  (should-not (sumibi--is-english-word "watashi"))
  (should-not (sumibi--is-english-word "nihongo"))
  (should-not (sumibi--is-english-word "desu")))

;;; Tests for sumibi-is-english-text-p

(ert-deftest test-is-english-text-pure-english ()
  "Test that pure English text is detected."
  (should (sumibi-is-english-text-p "I will not create a pull request"))
  (should (sumibi-is-english-text-p "the quick brown fox jumps over the lazy dog"))
  (should (sumibi-is-english-text-p "this is a test")))

(ert-deftest test-is-english-text-pure-romaji ()
  "Test that pure romaji text is not detected as English."
  (should-not (sumibi-is-english-text-p "watashiwa nihongo"))
  (should-not (sumibi-is-english-text-p "tokyode tabemasu"))
  (should-not (sumibi-is-english-text-p "arigatou gozaimasu")))

(ert-deftest test-is-english-text-threshold ()
  "Test the 80% threshold for English detection."
  ;; 5 words: 4 English (80%) = boundary case, should be detected
  (should (sumibi-is-english-text-p "I will not create watashi"))

  ;; 5 words: 3 English (60%) = below threshold, should not be detected
  (should-not (sumibi-is-english-text-p "I will watashi nihongo desu")))

(ert-deftest test-is-english-text-empty-string ()
  "Test that empty string returns nil."
  (should-not (sumibi-is-english-text-p ""))
  (should-not (sumibi-is-english-text-p nil)))

(ert-deftest test-is-english-text-single-word ()
  "Test single word detection."
  (should (sumibi-is-english-text-p "hello"))
  (should-not (sumibi-is-english-text-p "watashi")))

(ert-deftest test-is-english-text-with-punctuation ()
  "Test that punctuation doesn't affect detection."
  ;; Note: split-string removes punctuation, so we test words only
  (should (sumibi-is-english-text-p "I will not create a pull request")))

(ert-deftest test-is-english-text-mixed-case ()
  "Test mixed case English text."
  (should (sumibi-is-english-text-p "The Quick Brown Fox"))
  (should (sumibi-is-english-text-p "THIS IS A TEST")))

;;; Tests for apostrophe handling

(ert-deftest test-is-english-text-with-apostrophe ()
  "Test that English text with apostrophes is detected correctly."
  (should (sumibi-is-english-text-p "that's what our new LLMFunction is about"))
  (should (sumibi-is-english-text-p "it's a new day"))
  (should (sumibi-is-english-text-p "I don't think we can do that"))
  (should (sumibi-is-english-text-p "you're welcome to join us")))

(ert-deftest test-remove-apostrophe ()
  "Test apostrophe removal function."
  (should (equal "thats" (sumibi--remove-apostrophe "that's")))
  (should (equal "its" (sumibi--remove-apostrophe "it's")))
  (should (equal "dont" (sumibi--remove-apostrophe "don't")))
  (should (equal "cant" (sumibi--remove-apostrophe "can't")))
  (should (equal "youre" (sumibi--remove-apostrophe "you're")))
  (should (equal "hello" (sumibi--remove-apostrophe "hello"))))

;;; Tests for punctuation handling

(ert-deftest test-normalize-word ()
  "Test word normalization function."
  (should (equal "hello" (sumibi--normalize-word "hello,")))
  (should (equal "world" (sumibi--normalize-word "world.")))
  (should (equal "test" (sumibi--normalize-word "test?")))
  (should (equal "package" (sumibi--normalize-word "\"package")))
  (should (equal "up" (sumibi--normalize-word "up\"")))
  (should (equal "hello" (sumibi--normalize-word "hello"))))

(ert-deftest test-is-english-text-with-punctuation-marks ()
  "Test that English text with punctuation marks is detected correctly."
  (should (sumibi-is-english-text-p "Hello, world. How are you?"))
  (should (sumibi-is-english-text-p "I will not create a pull request."))
  (should (sumibi-is-english-text-p "So far, we mostly think of LLMs as things we interact directly with"))
  (should (sumibi-is-english-text-p "that's what our new LLMFunction is about")))

;;; Tests for contractions handling

(ert-deftest test-is-english-contraction ()
  "Test that contractions are recognized correctly."
  (should (sumibi--is-english-contraction-p "don't"))
  (should (sumibi--is-english-contraction-p "we've"))
  (should (sumibi--is-english-contraction-p "I'm"))
  (should (sumibi--is-english-contraction-p "you're"))
  (should (sumibi--is-english-contraction-p "it's"))
  (should-not (sumibi--is-english-contraction-p "hello")))

(ert-deftest test-is-english-text-with-contractions ()
  "Test that English text with contractions is detected correctly."
  (should (sumibi-is-english-text-p "I don't think we've seen anything like this before"))
  (should (sumibi-is-english-text-p "It's amazing how AI can understand natural language"))
  (should (sumibi-is-english-text-p "You're going to love this new feature")))

(provide 'sumibi-english-detection-test)
;;; sumibi-english-detection-test.el ends here
