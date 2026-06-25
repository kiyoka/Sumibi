;;; sumibi-mozc.el --- mozc_emacs_helper client for provisional conversion -*- lexical-binding: t; -*-
;;
;; -*- indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2026 Kiyoka Nishiyama
;;
;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Keywords: lisp, ime, japanese, mozc
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
;;

;;; Commentary:

;; mozc_emacs_helper を常駐プロセスとして 1 本だけ起動し、ひらがな文字列を
;; かな漢字変換する軽量クライアントです。Sumibi の二段階変換 (Issue #162) で、
;; LLM 変換の完了を待つ間の「仮確定」をローカルかつ即時に得るために使用します。
;;
;; mozc_emacs_helper のS式プロトコル:
;;   (0 CreateSession)
;;   (1 SendKey 1 "あ") ... 1 文字ずつ
;;   (n SendKey 1 space)   ; 変換
;;   (n SendKey 1 return)  ; 確定 → (result . ((type . string)(value . "...")...))
;;
;; mozc はローカルで動作し応答が速い (通常 1ms オーダー) ため、本クライアントは
;; 同期的に応答を読み取ります。失敗・タイムアウト時は nil を返し、呼び出し側が
;; フォールバックできるようにします。

;;; Code:

(require 'cl-lib)
(require 'seq)

(defgroup sumibi-mozc nil
  "mozc_emacs_helper を使った仮確定変換の設定."
  :group 'sumibi)

(defcustom sumibi-mozc-helper-path nil
  "mozc_emacs_helper の実行パス.
nil の場合は `sumibi-mozc--helper-candidates' から自動検出する."
  :type '(choice (const :tag "自動検出" nil)
                 (file :tag "パスを指定"))
  :group 'sumibi-mozc)

(defcustom sumibi-mozc-timeout 1.0
  "mozc_emacs_helper の応答待ちタイムアウト秒数."
  :type 'number
  :group 'sumibi-mozc)

(defconst sumibi-mozc--helper-candidates
  '("mozc_emacs_helper"                          ; PATH 上
    "/usr/bin/mozc_emacs_helper"                 ; Linux (一部ディストロ)
    "/usr/lib/mozc/mozc_emacs_helper"            ; Linux (Debian/Ubuntu)
    "/usr/lib64/mozc/mozc_emacs_helper"          ; Linux (Fedora)
    "/usr/local/lib/mozc/mozc_emacs_helper"      ; 手動インストール
    "/opt/homebrew/lib/mozc/mozc_emacs_helper"   ; macOS (Homebrew ARM)
    "/opt/homebrew/bin/mozc_emacs_helper"        ; macOS (Homebrew ARM bin)
    "/usr/local/bin/mozc_emacs_helper")          ; macOS (Homebrew Intel bin)
  "mozc_emacs_helper の探索候補パス.")

(defvar sumibi-mozc--process nil
  "常駐している mozc_emacs_helper のプロセスオブジェクト.")
(defvar sumibi-mozc--accumulator ""
  "プロセスフィルタが受信した出力を蓄積する文字列.")
(defvar sumibi-mozc--event-id 0
  "S式コマンドに付与する単調増加のイベントID.")
(defvar sumibi-mozc--session-id nil
  "CreateSession で得たセッションID (全変換で使い回す).")

(defun sumibi-mozc-find-helper ()
  "mozc_emacs_helper の実行パスを返す。見つからなければ nil."
  (or (and sumibi-mozc-helper-path
           (file-executable-p sumibi-mozc-helper-path)
           sumibi-mozc-helper-path)
      (cl-some (lambda (p)
                 (if (file-name-absolute-p p)
                     (and (file-executable-p p) p)
                   (executable-find p)))
               sumibi-mozc--helper-candidates)))

(defun sumibi-mozc-available-p ()
  "mozc_emacs_helper が利用可能なら t を返す."
  (and (sumibi-mozc-find-helper) t))

(defun sumibi-mozc--next-id ()
  "次のイベントIDを返す."
  (setq sumibi-mozc--event-id (1+ sumibi-mozc--event-id)))

(defun sumibi-mozc--filter (_proc string)
  "プロセス PROC の出力 STRING を蓄積するフィルタ関数."
  (setq sumibi-mozc--accumulator (concat sumibi-mozc--accumulator string)))

(defun sumibi-mozc--quote-key (ch)
  "文字 CH を SendKey 用の \"...\" 文字列リテラルに変換する."
  (let ((s (char-to-string ch)))
    (cond
     ((string= s "\"") "\"\\\"\"")
     ((string= s "\\") "\"\\\\\"")
     (t (format "\"%s\"" s)))))

(defun sumibi-mozc--extract-session-id ()
  "蓄積出力から CreateSession の emacs-session-id を取り出す。無ければ nil."
  (when (string-match "(emacs-session-id \\. \\([0-9]+\\))" sumibi-mozc--accumulator)
    (string-to-number (match-string 1 sumibi-mozc--accumulator))))

(defun sumibi-mozc--extract-result ()
  "蓄積出力から確定結果 (result の value) を取り出す。無ければ nil."
  (when (string-match
         "(result \\. ((type \\. string)(value \\. \"\\([^\"]*\\)\")"
         sumibi-mozc--accumulator)
    (match-string 1 sumibi-mozc--accumulator)))

(defun sumibi-mozc-shutdown ()
  "常駐プロセスを停止し、状態を初期化する."
  (when (and sumibi-mozc--process
             (process-live-p sumibi-mozc--process))
    (ignore-errors (delete-process sumibi-mozc--process)))
  (setq sumibi-mozc--process nil
        sumibi-mozc--session-id nil
        sumibi-mozc--accumulator ""))

(defun sumibi-mozc--ensure-process ()
  "常駐プロセスとセッションを確保する。成功したら t を返す."
  (if (and sumibi-mozc--process
           (process-live-p sumibi-mozc--process)
           sumibi-mozc--session-id)
      t
    (let ((helper (sumibi-mozc-find-helper)))
      (when helper
        (condition-case _err
            (progn
              (sumibi-mozc-shutdown)
              (setq sumibi-mozc--accumulator ""
                    sumibi-mozc--event-id 0
                    sumibi-mozc--session-id nil)
              (let ((proc (make-process
                           :name "sumibi-mozc"
                           :command (list helper)
                           :coding 'utf-8
                           :connection-type 'pipe
                           :noquery t
                           :filter #'sumibi-mozc--filter)))
                (setq sumibi-mozc--process proc)
                (process-send-string proc
                                     (format "(%d CreateSession)\n"
                                             (sumibi-mozc--next-id)))
                (let ((deadline (+ (float-time) sumibi-mozc-timeout)))
                  (while (and (not (sumibi-mozc--extract-session-id))
                              (< (float-time) deadline)
                              (process-live-p proc))
                    (accept-process-output proc 0.05)))
                (if (setq sumibi-mozc--session-id (sumibi-mozc--extract-session-id))
                    t
                  (sumibi-mozc-shutdown)
                  nil)))
          (error
           (sumibi-mozc-shutdown)
           nil))))))

(defun sumibi-mozc-convert (hiragana)
  "ひらがな文字列 HIRAGANA を mozc でかな漢字変換し、第1候補を返す.
変換できない場合や mozc が利用できない場合は nil を返す."
  (when (and (stringp hiragana)
             (> (length hiragana) 0)
             (sumibi-mozc--ensure-process))
    (condition-case _err
        (let ((proc sumibi-mozc--process)
              (sid sumibi-mozc--session-id)
              (cmds '()))
          (setq sumibi-mozc--accumulator "")
          ;; ひらがなを1文字ずつ送信
          (dolist (ch (string-to-list hiragana))
            (push (format "(%d SendKey %d %s)"
                          (sumibi-mozc--next-id) sid
                          (sumibi-mozc--quote-key ch))
                  cmds))
          ;; スペースで変換、returnで確定
          (push (format "(%d SendKey %d space)" (sumibi-mozc--next-id) sid) cmds)
          (push (format "(%d SendKey %d return)" (sumibi-mozc--next-id) sid) cmds)
          (process-send-string proc
                               (concat (mapconcat #'identity (nreverse cmds) "\n") "\n"))
          ;; 確定結果を同期的に待つ
          (let ((deadline (+ (float-time) sumibi-mozc-timeout))
                (value nil))
            (while (and (not (setq value (sumibi-mozc--extract-result)))
                        (< (float-time) deadline)
                        (process-live-p proc))
              (accept-process-output proc 0.05))
            (if value
                value
              ;; タイムアウト時はセッション状態がずれている可能性があるため再起動
              (sumibi-mozc-shutdown)
              nil)))
      (error
       (sumibi-mozc-shutdown)
       nil))))

(provide 'sumibi-mozc)

;;; sumibi-mozc.el ends here
