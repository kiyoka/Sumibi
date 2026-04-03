;;; sumibi-chinese-test.el --- Tests for sumibi-chinese -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama

;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: chinese, ime, pinyin

;;; Commentary:
;; sumibi-chinese.el の単体テスト。
;; LLM API呼び出しを伴わない関数のみテストする。

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

(require 'sumibi-chinese)

;; ------------------------------------------------------------------
;; sumibi-chinese-character-set のデフォルト値テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-character-set-default ()
  "デフォルトの文字セットが simplified であることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (should (eq sumibi-chinese-character-set 'simplified))))

;; ------------------------------------------------------------------
;; システムプロンプト切替テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-system-prompt-simplified ()
  "simplified 設定時に簡体字用プロンプトが返ることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (should (string-match-p "简体字" (sumibi-chinese-system-prompt)))
    (should-not (string-match-p "繁體字" (sumibi-chinese-system-prompt)))))

(ert-deftest test-chinese-system-prompt-traditional ()
  "traditional 設定時に繁体字用プロンプトが返ることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    (should (string-match-p "繁體字" (sumibi-chinese-system-prompt)))
    (should-not (string-match-p "简体字" (sumibi-chinese-system-prompt)))))

;; ------------------------------------------------------------------
;; Few-shot 例切替テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-few-shot-simplified ()
  "simplified 設定時に簡体字の few-shot 例が返ることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (let ((examples (sumibi-chinese-few-shot-examples)))
      (should (listp examples))
      (should (> (length examples) 0))
      ;; 最初の例の回答が簡体字であること
      (should (string= "我是中国人" (cdar examples))))))

(ert-deftest test-chinese-few-shot-traditional ()
  "traditional 設定時に繁体字の few-shot 例が返ることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    (let ((examples (sumibi-chinese-few-shot-examples)))
      (should (listp examples))
      (should (> (length examples) 0))
      ;; 最初の例の回答が繁体字であること
      (should (string= "我是中國人" (cdar examples))))))

(ert-deftest test-chinese-few-shot-count ()
  "簡体字・繁体字の few-shot 例の数が同じであることを確認する。"
  (should (= (length sumibi-chinese-few-shot-simplified)
             (length sumibi-chinese-few-shot-traditional))))

(ert-deftest test-chinese-few-shot-tone-example-simplified ()
  "簡体字の声調数字 few-shot 例が含まれていることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (let* ((examples (sumibi-chinese-few-shot-examples))
           (answers (mapcar #'cdr examples)))
      ;; ma1 ma2 ma3 ma4 の回答が含まれること
      (should (member "妈麻马骂" answers)))))

(ert-deftest test-chinese-few-shot-tone-example-traditional ()
  "繁体字の声調数字 few-shot 例が含まれていることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    (let* ((examples (sumibi-chinese-few-shot-examples))
           (answers (mapcar #'cdr examples)))
      ;; ma1 ma2 ma3 ma4 の繁体字回答が含まれること
      (should (member "媽麻馬罵" answers)))))

;; ------------------------------------------------------------------
;; ユーザープロンプトテンプレート切替テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-user-prompt-template-simplified ()
  "simplified 設定時に簡体字用テンプレートが返ることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (let ((template (sumibi-chinese-user-prompt-template)))
      (should (string-match-p "请将以下拼音转换为中文" template))
      (should (string-match-p "%s" template)))))

(ert-deftest test-chinese-user-prompt-template-traditional ()
  "traditional 設定時に繁体字用テンプレートが返ることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    (let ((template (sumibi-chinese-user-prompt-template)))
      (should (string-match-p "請將以下拼音轉換為中文" template))
      (should (string-match-p "%s" template)))))

;; ------------------------------------------------------------------
;; モードライン文字列テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-mode-line-simplified ()
  "simplified 設定時にモードラインが [简] であることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    (should (string= " [简]" (sumibi-chinese-mode-line-string)))))

(ert-deftest test-chinese-mode-line-traditional ()
  "traditional 設定時にモードラインが [繁] であることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    (should (string= " [繁]" (sumibi-chinese-mode-line-string)))))

;; ------------------------------------------------------------------
;; skip-chars のテスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-skip-chars-backward-pinyin ()
  "ピンイン文字列を後方スキップできることを確認する。"
  (with-temp-buffer
    (insert "wo shi zhongguo ren")
    (should (< (sumibi-chinese-skip-chars-backward) 0))))

(ert-deftest test-chinese-skip-chars-backward-with-tone ()
  "声調数字付きピンインを後方スキップできることを確認する。"
  (with-temp-buffer
    (insert "ma1 ma2 ma3 ma4")
    (should (< (sumibi-chinese-skip-chars-backward) 0))))

(ert-deftest test-chinese-skip-chars-backward-empty ()
  "空バッファでスキップが0であることを確認する。"
  (with-temp-buffer
    (should (= 0 (sumibi-chinese-skip-chars-backward)))))

(ert-deftest test-chinese-skip-chars-stops-at-chinese ()
  "中国語文字の前でスキップが止まることを確認する。"
  (with-temp-buffer
    (insert "你好world")
    (let ((gap (sumibi-chinese-skip-chars-backward)))
      ;; "world" (5文字) 分だけ戻る
      (should (= gap -5)))))

;; ------------------------------------------------------------------
;; 候補数決定テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-determine-number-short ()
  "短いテキストでは候補数が3であることを確認する。"
  (should (= 3 (sumibi-chinese-determine-number-of-n "ni hao"))))

(ert-deftest test-chinese-determine-number-long ()
  "長いテキストでは候補数が1であることを確認する。"
  (let ((long-text (make-string (1+ sumibi-threshold-letters-of-long-sentence) ?a)))
    (should (= 1 (sumibi-chinese-determine-number-of-n long-text)))))

;; ------------------------------------------------------------------
;; 設定の整合性テスト
;; ------------------------------------------------------------------

(ert-deftest test-chinese-simplified-prompt-consistency ()
  "簡体字プロンプトとfew-shot例の言語が一致していることを確認する。"
  (let ((sumibi-chinese-character-set 'simplified))
    ;; プロンプトに「简体字」が含まれ、few-shotの回答も簡体字
    (should (string-match-p "简体字" (sumibi-chinese-system-prompt)))
    (let ((first-answer (cdar (sumibi-chinese-few-shot-examples))))
      ;; 「国」は簡体字、「國」は繁体字
      (should (string-match-p "国" first-answer))
      (should-not (string-match-p "國" first-answer)))))

(ert-deftest test-chinese-traditional-prompt-consistency ()
  "繁体字プロンプトとfew-shot例の言語が一致していることを確認する。"
  (let ((sumibi-chinese-character-set 'traditional))
    ;; プロンプトに「繁體字」が含まれ、few-shotの回答も繁体字
    (should (string-match-p "繁體字" (sumibi-chinese-system-prompt)))
    (let ((first-answer (cdar (sumibi-chinese-few-shot-examples))))
      ;; 「國」は繁体字
      (should (string-match-p "國" first-answer)))))

(provide 'sumibi-chinese-test)
;;; sumibi-chinese-test.el ends here
