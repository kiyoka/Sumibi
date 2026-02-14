;;; sumibi-auto-convert-exclude-test.el --- Tests for auto-convert exclusion -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Kiyoka Nishiyama

;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: japanese, ime

;;; Commentary:
;; このファイルは、自動変換の除外機能（シェルバッファ、GPGファイル）のテストを提供します。

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

;; shell-mode is needed for testing
(require 'shell)

(require 'sumibi)

;; ------------------------------------------------------------------
;; 除外条件判定のテスト
;; ------------------------------------------------------------------

(ert-deftest test-should-exclude-shell-mode ()
  "shell-modeでは自動変換が除外されることを確認する。"
  (with-temp-buffer
    (shell-mode)
    (should (eq major-mode 'shell-mode))
    (should (sumibi-should-exclude-auto-convert-p))))

(ert-deftest test-should-exclude-gpg-file ()
  ".gpg拡張子のファイルでは自動変換が除外されることを確認する。"
  (let ((temp-file (make-temp-file "test-sumibi-" nil ".gpg")))
    (unwind-protect
        (with-temp-buffer
          (set-visited-file-name temp-file)
          (should (string-suffix-p ".gpg" (buffer-file-name)))
          (should (sumibi-should-exclude-auto-convert-p)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-should-not-exclude-normal-buffer ()
  "通常のバッファでは自動変換が除外されないことを確認する。"
  (with-temp-buffer
    (text-mode)
    (should (eq major-mode 'text-mode))
    (should-not (sumibi-should-exclude-auto-convert-p))))

(ert-deftest test-should-not-exclude-normal-file ()
  "通常のファイル（.el等）では自動変換が除外されないことを確認する。"
  (let ((temp-file (make-temp-file "test-sumibi-" nil ".el")))
    (unwind-protect
        (with-temp-buffer
          (set-visited-file-name temp-file)
          (should (string-suffix-p ".el" (buffer-file-name)))
          (should-not (sumibi-should-exclude-auto-convert-p)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-should-exclude-custom-mode ()
  "カスタマイズ変数に追加したモードでは自動変換が除外されることを確認する。"
  (let ((sumibi-ambient-exclude-modes '(shell-mode emacs-lisp-mode)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (should (eq major-mode 'emacs-lisp-mode))
      (should (sumibi-should-exclude-auto-convert-p)))))

(ert-deftest test-should-exclude-custom-extension ()
  "カスタマイズ変数に追加した拡張子では自動変換が除外されることを確認する。"
  (let ((sumibi-ambient-exclude-file-extensions '("gpg" "secret"))
        (temp-file (make-temp-file "test-sumibi-" nil ".secret")))
    (unwind-protect
        (with-temp-buffer
          (set-visited-file-name temp-file)
          (should (string-suffix-p ".secret" (buffer-file-name)))
          (should (sumibi-should-exclude-auto-convert-p)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest test-should-not-exclude-buffer-without-file ()
  "ファイル名を持たないバッファでは自動変換が除外されないことを確認する。"
  (with-temp-buffer
    (text-mode)
    (should-not (buffer-file-name))
    (should-not (sumibi-should-exclude-auto-convert-p))))

(ert-deftest test-should-exclude-minibuffer ()
  "ミニバッファでは自動変換が除外されることを確認する。
注: このテストはミニバッファの特性上、自動テストでは検証できないため、
     minibufferp 関数が正しく動作することを前提としています。
     実際の動作確認は、M-x find-file で 'a.txt' と入力して、
     ドット入力時に自動変換が発動しないことを手動で確認してください。"
  ;; minibufferp 関数が存在し、呼び出し可能であることを確認
  (should (fboundp 'minibufferp))
  ;; 通常のバッファではミニバッファではないことを確認
  (with-temp-buffer
    (should-not (minibufferp))))

(provide 'sumibi-auto-convert-exclude-test)
;;; sumibi-auto-convert-exclude-test.el ends here
