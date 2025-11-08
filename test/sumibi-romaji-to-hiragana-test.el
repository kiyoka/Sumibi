;;; sumibi-romaji-to-hiragana-test.el --- Tests for romaji-to-hiragana conversion -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: tests

;;; Commentary:

;; Unit tests for sumibi-romaji-to-hiragana function (Issue #97)

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

;;; Tests for basic conversions

(ert-deftest test-romaji-to-hiragana-basic-vowels ()
  "Test basic vowel conversion."
  (should (string= "あいうえお" (sumibi-romaji-to-hiragana "aiueo")))
  (should (string= "あ" (sumibi-romaji-to-hiragana "a")))
  (should (string= "い" (sumibi-romaji-to-hiragana "i")))
  (should (string= "う" (sumibi-romaji-to-hiragana "u")))
  (should (string= "え" (sumibi-romaji-to-hiragana "e")))
  (should (string= "お" (sumibi-romaji-to-hiragana "o"))))

(ert-deftest test-romaji-to-hiragana-basic-consonants ()
  "Test basic consonant combinations."
  (should (string= "かきくけこ" (sumibi-romaji-to-hiragana "kakikukeko")))
  (should (string= "さしすせそ" (sumibi-romaji-to-hiragana "sashisuseso")))
  (should (string= "たちつてと" (sumibi-romaji-to-hiragana "tachitsuteto")))
  (should (string= "なにぬねの" (sumibi-romaji-to-hiragana "naninuneno")))
  (should (string= "はひふへほ" (sumibi-romaji-to-hiragana "hahifuheho"))))

(ert-deftest test-romaji-to-hiragana-common-words ()
  "Test common Japanese word conversions."
  (should (string= "わたし" (sumibi-romaji-to-hiragana "watashi")))
  (should (string= "にほん" (sumibi-romaji-to-hiragana "nihon")))
  (should (string= "おはよう" (sumibi-romaji-to-hiragana "ohayou")))
  (should (string= "ありがとう" (sumibi-romaji-to-hiragana "arigatou")))
  (should (string= "さよなら" (sumibi-romaji-to-hiragana "sayonara")))
  (should (string= "こんにちは" (sumibi-romaji-to-hiragana "konnitiha"))))

;;; Tests for special characters

(ert-deftest test-romaji-to-hiragana-youon ()
  "Test youon (contracted sounds) conversion."
  (should (string= "きょう" (sumibi-romaji-to-hiragana "kyou")))
  (should (string= "しゃしん" (sumibi-romaji-to-hiragana "shashin")))
  (should (string= "ちゅうがっこう" (sumibi-romaji-to-hiragana "chuugakkou")))
  (should (string= "にゅうがく" (sumibi-romaji-to-hiragana "nyuugaku")))
  (should (string= "ひゃっか" (sumibi-romaji-to-hiragana "hyakka")))
  (should (string= "みょうじ" (sumibi-romaji-to-hiragana "myouji")))
  (should (string= "りょこう" (sumibi-romaji-to-hiragana "ryokou"))))

(ert-deftest test-romaji-to-hiragana-sokuon ()
  "Test sokuon (geminate consonants) conversion."
  (should (string= "きって" (sumibi-romaji-to-hiragana "kitte")))
  (should (string= "がっこう" (sumibi-romaji-to-hiragana "gakkou")))
  (should (string= "にっぽん" (sumibi-romaji-to-hiragana "nippon")))
  (should (string= "ざっし" (sumibi-romaji-to-hiragana "zasshi")))
  (should (string= "こっぷ" (sumibi-romaji-to-hiragana "koppu")))
  (should (string= "まっちゃ" (sumibi-romaji-to-hiragana "maccha")))
  (should (string= "ちょっと" (sumibi-romaji-to-hiragana "chotto"))))

(ert-deftest test-romaji-to-hiragana-n ()
  "Test 'n' sound conversion."
  (should (string= "ん" (sumibi-romaji-to-hiragana "nn")))
  (should (string= "せんせい" (sumibi-romaji-to-hiragana "sensei")))
  (should (string= "ほん" (sumibi-romaji-to-hiragana "hon")))
  (should (string= "さんぽ" (sumibi-romaji-to-hiragana "sanpo")))
  (should (string= "てんき" (sumibi-romaji-to-hiragana "tenki"))))

(ert-deftest test-romaji-to-hiragana-long-vowel ()
  "Test long vowel (chouon) conversion."
  (should (string= "おーさか" (sumibi-romaji-to-hiragana "o-saka")))
  (should (string= "こーひー" (sumibi-romaji-to-hiragana "ko-hi-")))
  (should (string= "らーめん" (sumibi-romaji-to-hiragana "ra-men"))))

;;; Tests for special cases

(ert-deftest test-romaji-to-hiragana-invalid-sequence ()
  "Test handling of invalid romaji sequences."
  ;; Invalid sequences: if any part is unconvertible, preserve entire string
  (should (string= "shimasit" (sumibi-romaji-to-hiragana "shimasit")))
  (should (string= "axyz" (sumibi-romaji-to-hiragana "axyz")))
  (should (string= "tesutoq" (sumibi-romaji-to-hiragana "tesutoq"))))

(ert-deftest test-romaji-to-hiragana-case-insensitive ()
  "Test case-insensitive conversion."
  (should (string= "わたし" (sumibi-romaji-to-hiragana "WATASHI")))
  (should (string= "わたし" (sumibi-romaji-to-hiragana "Watashi")))
  (should (string= "わたし" (sumibi-romaji-to-hiragana "WaTaShI"))))

(ert-deftest test-romaji-to-hiragana-empty-string ()
  "Test empty string conversion."
  (should (string= "" (sumibi-romaji-to-hiragana ""))))

(ert-deftest test-romaji-to-hiragana-special-variants ()
  "Test special romaji variants (shi/si, chi/ti, tsu/tu, etc)."
  (should (string= "し" (sumibi-romaji-to-hiragana "si")))
  (should (string= "し" (sumibi-romaji-to-hiragana "shi")))
  (should (string= "ち" (sumibi-romaji-to-hiragana "ti")))
  (should (string= "ち" (sumibi-romaji-to-hiragana "chi")))
  (should (string= "つ" (sumibi-romaji-to-hiragana "tu")))
  (should (string= "つ" (sumibi-romaji-to-hiragana "tsu")))
  (should (string= "ふ" (sumibi-romaji-to-hiragana "hu")))
  (should (string= "ふ" (sumibi-romaji-to-hiragana "fu"))))

;;; Tests for complex sentences

(ert-deftest test-romaji-to-hiragana-complex-sentence-1 ()
  "Test complex sentence conversion (example 1)."
  (should (string= "わたしのなまえはなかのです"
                   (sumibi-romaji-to-hiragana "watashinonamaehanakanodesu"))))

(ert-deftest test-romaji-to-hiragana-complex-sentence-2 ()
  "Test complex sentence conversion (example 2)."
  (should (string= "きょうはいいてんきですね"
                   (sumibi-romaji-to-hiragana "kyouhaiitenkidesune"))))

(ert-deftest test-romaji-to-hiragana-complex-sentence-3 ()
  "Test complex sentence conversion (example 3)."
  (should (string= "にほんごをべんきょうします"
                   (sumibi-romaji-to-hiragana "nihongowobenkyoushimasu"))))

;;; Tests for dakuten and handakuten

(ert-deftest test-romaji-to-hiragana-dakuten ()
  "Test dakuten (voiced) characters."
  (should (string= "がぎぐげご" (sumibi-romaji-to-hiragana "gagigugego")))
  (should (string= "ざじずぜぞ" (sumibi-romaji-to-hiragana "zazizuzezo")))
  (should (string= "だぢづでど" (sumibi-romaji-to-hiragana "dadidudedo")))
  (should (string= "ばびぶべぼ" (sumibi-romaji-to-hiragana "babibubebo"))))

(ert-deftest test-romaji-to-hiragana-handakuten ()
  "Test handakuten (semi-voiced) characters."
  (should (string= "ぱぴぷぺぽ" (sumibi-romaji-to-hiragana "papipupepo"))))

;;; Tests for Issue #97 requirements

(ert-deftest test-romaji-to-hiragana-preserve-unconvertible ()
  "Test that unconvertible characters are preserved (Issue #97 requirement)."
  ;; English words are converted to hiragana (preserve-english is not yet implemented)
  ;; "hello" -> "he" + "ll" + "o" -> "へ" + "っ" + "l" + "お"
  ;; Note: This is expected behavior until preserve-english feature is implemented

  ;; Invalid sequences are preserved
  (should (string= "xyz" (sumibi-romaji-to-hiragana "xyz")))

  ;; Mixed valid and invalid - entire string is preserved
  (should (string= "axyz" (sumibi-romaji-to-hiragana "axyz"))))

(ert-deftest test-romaji-to-hiragana-benchmark-examples ()
  "Test examples from Issue #96 benchmark."
  ;; These are typical inputs from the benchmark (without spaces)
  (should (string= "わたしのなまえはにしやまです"
                   (sumibi-romaji-to-hiragana "watashinonamaehanishiyamadesu")))
  (should (string= "きょうはいいてんきですね"
                   (sumibi-romaji-to-hiragana "kyouhaiitenkidesune")))
  ;; Comma is unconvertible, so entire string is preserved
  (should (string= "konnitiha,genkidesuka"
                   (sumibi-romaji-to-hiragana "konnitiha,genkidesuka"))))

;;; Edge cases

(ert-deftest test-romaji-to-hiragana-single-consonant ()
  "Test single consonant handling."
  ;; Single 'n' should convert to ん
  (should (string= "ん" (sumibi-romaji-to-hiragana "n")))
  ;; Other single consonants should be preserved
  (should (string= "k" (sumibi-romaji-to-hiragana "k")))
  (should (string= "s" (sumibi-romaji-to-hiragana "s"))))

(ert-deftest test-romaji-to-hiragana-numbers-and-symbols ()
  "Test that numbers and symbols are preserved."
  (should (string= "123" (sumibi-romaji-to-hiragana "123")))
  (should (string= "!@#" (sumibi-romaji-to-hiragana "!@#")))
  ;; Numbers mixed with romaji: entire string is preserved
  (should (string= "watashi123" (sumibi-romaji-to-hiragana "watashi123"))))

;;; Tests for English word detection (preserve-english feature)

(ert-deftest test-romaji-to-hiragana-english-word-preservation ()
  "Test that English words are preserved when preserve-english is non-nil."
  ;; Common English words that should be in the dictionary (3-10 letters)
  (should (string= "test" (sumibi-romaji-to-hiragana "test" t)))
  (should (string= "hello" (sumibi-romaji-to-hiragana "hello" t)))
  (should (string= "world" (sumibi-romaji-to-hiragana "world" t)))
  (should (string= "about" (sumibi-romaji-to-hiragana "about" t)))
  (should (string= "think" (sumibi-romaji-to-hiragana "think" t)))
  ;; Longer words (9-10 letters)
  (should (string= "attention" (sumibi-romaji-to-hiragana "attention" t)))
  (should (string= "additional" (sumibi-romaji-to-hiragana "additional" t)))
  ;; Case insensitive - should still be preserved
  (should (string= "Test" (sumibi-romaji-to-hiragana "Test" t)))
  (should (string= "HELLO" (sumibi-romaji-to-hiragana "HELLO" t)))
  (should (string= "Attention" (sumibi-romaji-to-hiragana "Attention" t))))

(ert-deftest test-romaji-to-hiragana-english-without-preserve ()
  "Test that English words are converted when preserve-english is nil."
  ;; Without preserve-english, should try to convert
  ;; "test" -> "てst" but 's' cannot convert, so entire string preserved
  (should (string= "test" (sumibi-romaji-to-hiragana "test" nil)))
  ;; "hello" -> "へっlo" but 'l' cannot convert, so entire string preserved
  (should (string= "hello" (sumibi-romaji-to-hiragana "hello" nil))))

(ert-deftest test-romaji-to-hiragana-non-dictionary-words ()
  "Test words not in dictionary are handled correctly."
  ;; Words not in dictionary but convertible as romaji
  ;; "wikipedia" is not in dictionary, but all characters are convertible
  (should (string= "うぃきぺぢあ" (sumibi-romaji-to-hiragana "wikipedia" t)))
  ;; Short words not in basic dictionary and not convertible
  (should (string= "xyz" (sumibi-romaji-to-hiragana "xyz" t))))

(ert-deftest test-romaji-to-hiragana-japanese-not-affected ()
  "Test that Japanese romaji conversion is not affected by preserve-english."
  ;; Japanese words should convert regardless of preserve-english flag
  (should (string= "わたし" (sumibi-romaji-to-hiragana "watashi" t)))
  (should (string= "わたし" (sumibi-romaji-to-hiragana "watashi" nil)))
  (should (string= "きょう" (sumibi-romaji-to-hiragana "kyou" t)))
  (should (string= "きょう" (sumibi-romaji-to-hiragana "kyou" nil))))

(provide 'sumibi-romaji-to-hiragana-test)
;;; sumibi-romaji-to-hiragana-test.el ends here
