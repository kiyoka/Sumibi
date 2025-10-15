;;; sumibi.el --- Japanese input method powered by ChatGPT API   -*- lexical-binding: t; -*-
;;
;; -*- indent-tabs-mode: nil -*-
;;
;; Copyright (C) 2023 Kiyoka Nishiyama
;;
;; Author: Kiyoka Nishiyama <kiyoka@sumibi.org>
;; Version: 3.7.0
;; Keywords: lisp, ime, japanese
;; Package-Requires: ((emacs "29.0") (popup "0.5.9") (unicode-escape "1.1") (deferred "0.5.1") (mozc))
;; URL: https://github.com/kiyoka/Sumibi
;;
;; This file is part of Sumibi
;; This program was derived from sekka.el and yc.el-4.0.13(auther: knak)
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

;; you might want to enable IME:
;;
;;  (require 'sumibi)
;;  (global-sumibi-mode 1)
;;
;;

;;; Code:
(require 'cl-lib)
(require 'popup)
(require 'url)
(require 'url-http)
(require 'unicode-escape)
(require 'deferred)
(require 'sumibi-localdic)

;; --------------------------------------------------------------
;; Optional: use mozc.el as a local backend when the model name
;; `mozc' is specified.
;; --------------------------------------------------------------

(eval-when-compile (require 'cl-lib))

(defvar sumibi--mozc-available-p (require 'mozc nil 'noerror)
  "Non-nil if `mozc.el' could be loaded successfully.")

(defun sumibi-mozc--candidate-list (roman arg-n)
  "Return up to ARG-N candidate strings for ROMAN using mozc.

If mozc.el is unavailable, or Mozc raises any error, a list containing
ROMAN itself is returned so that callers can safely fall back."
  (if (not sumibi--mozc-available-p)
      (list roman)
    (setq roman (downcase roman))
    (condition-case _err
        (if (string-match-p "[ \t]" roman)
            ;; 空白で分割 → 各セグメントを再帰的に1件だけ変換 → つなげて返す
            (progn
              (let* ((split-words (split-string roman "[ \t]+" t))
                     (joined (apply #'concat
                                    (mapcar (lambda (w)
                                              (car (sumibi-mozc--candidate-list w 1)))
                                            split-words))))
                (list (propertize joined 'sumibi-mozc-candidate t))))
          ;; セグメント1件のときは従来ロジック
          (progn
            (mozc-session-create t)
            (dolist (ch (string-to-list roman))
              (mozc-session-sendkey (list ch)))
	    
            (let* ((iteration 0) resp cands)
              (while (and (< iteration 3)
                          (progn
                            (setq resp (mozc-session-sendkey '(space)))
                            (setq cands (and resp (mozc-protobuf-get resp 'candidates)))
                            (null cands)))
                (setq iteration (1+ iteration)))
	      
              (if (not cands)
                  (progn
                    (list roman))
                (progn
                  (let* ((cand-list (mozc-protobuf-get cands 'candidate))
                         ;; 候補の文字列リスト
                         (values   (mapcar (lambda (cand)
                                             (mozc-protobuf-get cand 'value))
                                           cand-list))
                         ;; annotation の description（カタカナ読み）
                         (raw-anno  (mozc-protobuf-get (nth 0 cand-list) 'annotation))
                         (anno-desc (and raw-anno (mozc-protobuf-get raw-anno 'description)))
                         (kata      anno-desc)
                         ;; ひらがなに変換
                         (hira      (and kata (sumibi-katakana-to-hiragana kata))))
                    ;; 候補 + ひらがな読み + カタカナ読み
                    (let* ((lst (append values (delq nil (list hira kata))))
                           ;; 履歴を考慮して候補順序を調整
                           (reordered (sumibi-mozc--find-preferred-candidate roman lst)))
                      ;; 各候補に origin プロパティを付与して返す - type check debug
                      (mapcar (lambda (s) 
                                (if (stringp s)
                                    (propertize s 'sumibi-mozc-candidate t)
                                  (progn
                                    (propertize (format "%s" s) 'sumibi-mozc-candidate t))))
                              reordered))))))))
      ;; error path ----------------------------------------------------
      (error
       (list roman)))))

(defun sumibi-mozc--find-preferred-candidate (roman candidates)
  "履歴からROMANに対応する過去の選択候補を探し、その候補を先頭に並び替える（genbunキーで検索）."
  (if (not sumibi-history-stack)
      (progn
        candidates)
    (let ((preferred-candidate nil))
      ;; 履歴から同じ原文入力の記録を探す
      (dolist (entry sumibi-history-stack)
        (when (not preferred-candidate)  ; まだ見つかっていない場合のみ
          (let ((genbun (sumibi-assoc-ref 'genbun entry nil)))
            (when (and genbun (equal roman genbun))
              (let* ((kouho-list (sumibi-assoc-ref 'henkan-kouho-list entry nil))
                     (cand-cur (sumibi-assoc-ref 'cand-cur entry nil)))
                ;; 過去に選択された候補を取得
                (when (and kouho-list cand-cur
                           (>= cand-cur 0)
                           (< cand-cur (length kouho-list)))
                  (let ((raw-candidate (car (nth cand-cur kouho-list))))
                    ;; 文字列であることを確認し、そうでなければ文字列に変換
                    (setq preferred-candidate (if (stringp raw-candidate)
                                                  raw-candidate
                                                (format "%s" raw-candidate))))))))))
      
      ;; 見つかった場合は、その候補を先頭に移動
      (if preferred-candidate
          (progn
            ;; 候補リストから該当候補を削除して先頭に追加
            (let ((filtered (remove preferred-candidate candidates))
                  (result))
              (setq result (cons preferred-candidate filtered))
              result))
        ;; 見つからない場合は元の順序のまま
        (progn
          candidates)))))

;;; 
;;;
;;; customize variables
;;;
(defgroup sumibi nil
  "Sumibi client."
  :group 'input-method
  :group 'Japanese)

(defcustom sumibi-stop-chars "(){}<>"
  "*漢字変換文字列を取り込む時に変換範囲に含めない文字を設定する."
  :type  'string
  :group 'sumibi)

;; ------------------------------------------------------------------
;; Utility: decide annotation label for a candidate string
;; ------------------------------------------------------------------
(defun sumibi--annotation-label (str idx)
  "Return annotation label for STR which is the (IDX+1)-th candidate.

If STR originated from `sumibi-mozc--candidate-list' the text property
`sumibi-mozc-candidate' is expected to be non-nil and the label will be
prefixed with \"Mozc\" so that users can recognise the source easily."
  (if (get-text-property 0 'sumibi-mozc-candidate str)
      (format "Mozc候補%d" idx)
    (format "候補%d" idx)))

;; ------------------------------------------------------------------
;; Teach Mozc the actually committed candidate (optional)
;; ------------------------------------------------------------------
(defcustom sumibi-mozc-learn-at-kakutei t
  "If non-nil, Sumibi asks Mozc to learn the candidate that was
finally committed in `sumibi-select-kakutei'.

This is achieved by spinning up a *separate* Mozc session at the
moment of confirmation, performing the same conversion again, moving
the selection to the committed candidate and sending an ENTER key so
that Mozc's adaptive learning mechanism records the choice.

The feature only works when `sumibi-backend' is `mozc'."
  :type 'boolean
  :group 'sumibi)

;; Internal helper ----------------------------------------------------
(defun sumibi--mozc-learn (roman committed)
  "Let Mozc learn that ROMAN converts to COMMITTED.

This function is called right after a candidate is confirmed via
`sumibi-select-kakutei'.  A *new* Mozc session is created so that the
learning operation is completely isolated from the session Sumibi
uses for ordinary candidate acquisition.

If COMMITTED cannot be found in Mozc's candidate list, the function
simply commits the default candidate so that at least the roman string
is registered in Mozc's history.  Any Mozc-related error is caught and
silently ignored so as not to interfere with the original Sumibi
workflow."
  (when (and sumibi--mozc-available-p            ; Mozc is loadable
             (sumibi-backend-mozc-p)             ; currently using Mozc backend
             (stringp roman) (stringp committed)
             (not (string-match-p "[ \t]" roman))) ; single segment only
    (condition-case err
        (progn
          ;; start isolated session
          (mozc-session-create t)

          ;; feed roman characters
          (dolist (ch (string-to-list (downcase roman)))
            (mozc-session-sendkey (list ch)))

          ;; send <space> up to 3 times until Mozc returns candidates
          (let* ((iteration 0)
                 resp cands)
            (while (and (< iteration 3)
                        (progn
                          (setq resp  (mozc-session-sendkey '(space)))
                          (setq cands (and resp (mozc-protobuf-get resp 'candidates)))
                          (null cands)))
              (setq iteration (1+ iteration)))

            (let* ((cand-list (and cands (mozc-protobuf-get cands 'candidate)))
                   (values    (and cand-list
                                   (mapcar (lambda (cand)
                                             (mozc-protobuf-get cand 'value))
                                           cand-list)))
                   (idx       (and values
                                   (cl-position committed values :test #'string=))))

              ;; The first space may have moved the selection to candidate 1.
              ;; Send UP once to ensure we are at candidate 0, then navigate.
              (when (and idx (> idx 0))
		(mozc-session-sendkey '(up))
		(dotimes (_ idx)
                  (mozc-session-sendkey '(down)))))
	    
            ;; Commit current candidate so that Mozc learns it.
            (mozc-session-sendkey '(enter)))

          ;; terminate session if supported
          (when (fboundp 'mozc-session-delete)
            (mozc-session-delete)))
      (error
       err))))

;; --------------------------------------------------------------
;; Backend selection for roman→kanji conversion.
;;   'openai (default) : use an OpenAI-compatible ChatCompletions API
;;   'mozc            : use local mozc.el session
;; --------------------------------------------------------------
(defcustom sumibi-backend 'openai
  "Backend engine used for *ローマ字→漢字かな混じり文* 変換.

openai : OpenAI だけでなく **OpenAI 互換** の ChatCompletions API
         (例: OpenAI, Google Gemini、ローカル LLM など) を利用する。
         利用するサービスは `SUMIBI_AI_BASEURL' で指定した URL に
         よって切り替えられます。
mozc   : ネットワークを使わずローカルの mozc.el で変換する。

読み仮名生成や翻訳など、ローマ字変換以外のルーチンは常に
OpenAI 互換 API を利用するため、この設定の影響を受けません。"
  :type '(choice (const :tag "OpenAI互換 API" openai)
                 (const :tag "Mozc (local)" mozc))
  :group 'sumibi)

(defun sumibi-backend-mozc-p ()
  "Return non-nil if `sumibi-backend' is `mozc'."
  (eq sumibi-backend 'mozc))

(defcustom sumibi-current-model "gpt-5"
  "使用する AI モデル名を指定する (デフォルトは gpt-5)。

この変数は OpenAI 互換 API に渡す **LLM モデル名** を示します。
OpenAI 互換 API を利用しない（ローマ字→漢字を mozc で処理したい）場合は
後述の `sumibi-backend' を `mozc' に設定してください。"
  :type  'string
  :group 'sumibi)

(defcustom sumibi-model-list '("gpt-5" "gpt-5-mini" "gpt-4.1" "gpt-4.1-mini" "gpt-4o" "gpt-4o-mini")
  "AI モデル名の候補を定義する (gpt-4 シリーズ以上)。"
  :type  '(repeat string)
  :group 'sumibi)

(defcustom sumibi-history-stack-limit 100
  "再度候補選択できる単語と場所を最大何件記憶するか."
  :type  'integer
  :group 'sumibi)

(defcustom sumibi-api-timeout 60
  "OpenAI 互換サーバーと通信する時のタイムアウトを指定する。(秒数)。"
  :type  'integer
  :group 'sumibi)

(defcustom sumibi-threshold-letters-of-long-sentence 100
  "OpenAI 互換サーバーに送信する際、長文として判断する文字数。
この文字数を超えると ChatCompletions API の引数 n を 1 に減らします。"
  :type  'integer
  :group 'sumibi)

(defcustom sumibi-surrounding-lines 6
  "変換対象の文字列の周辺の文章を何行分取り込むか."
  :type  'integer
  :group 'sumibi)

(defvar sumibi-mode nil             "漢字変換トグル変数.")


(defun sumibi-drop-right-slash (url)
  (if (string-suffix-p "/" url)
      (substring url 0 -1)
    url))

(defun sumibi-ai-base-url ()
  "利用中のAIエンドポイントのベースURLを返す。末尾のスラッシュは含まない.
SUMIBI_AI_BASEURL環境変数が未設定の場合はデフォルトURL\"https://api.openai.com/v1\"を返す.
環境変数に\"/v1\"が含まれている場合は、値から末尾のスラッシュを除去して返す.
それ以外の場合は、値から末尾のスラッシュを除去し、末尾に\"/v1\"を付加して返す."
  (let ((env (getenv "SUMIBI_AI_BASEURL")))
    (cond
     ((sumibi-backend-mozc-p)
      "mozc_server")
     ((not env)
      "https://api.openai.com/v1")
     ((string-match-p "/v1" env)
      (sumibi-drop-right-slash env))
     (t
      (concat 
       (sumibi-drop-right-slash env)
       "/v1")))))

(defun sumibi-ai-model ()
  "利用中のAIモデル名を返す."
  (if (sumibi-backend-mozc-p)
      "mozc"
    (or (getenv "SUMIBI_AI_MODEL") sumibi-current-model)))

(defun sumibi-gpt5-series-p ()
  "現在のモデルがGPT-5シリーズかどうかを判定する."
  (let ((model (sumibi-ai-model)))
    (and (stringp model)
         (string-match-p "\\`gpt-5" model))))

(defun sumibi-modeline-string ()
  "利用するモデル名を表示する."
  (format " Sumibi[%s|%s]" (sumibi-ai-base-url) (sumibi-ai-model)))

(defvar sumibi-select-mode nil      "候補選択モード変数.")
(or (assq 'sumibi-mode minor-mode-alist)
    (setq minor-mode-alist (cons
                            '(sumibi-mode (:eval (sumibi-modeline-string)))
                            minor-mode-alist)))


;; ローマ字漢字変換時、対象とするローマ字を設定するための変数
(defvar sumibi-skip-chars "a-zA-Z0-9.,@:`\\-+!\\[\\]?;' \t")
(defvar sumibi-rK-trans-key "\C-j"
  "*漢字変換キーを設定する.")
(defvar sumibi-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map sumibi-rK-trans-key 'sumibi-rK-trans)
    (define-key map "\M-j" 'sumibi-english-trans)
    map)
  "漢字変換トグルマップ.")
(defvar sumibi-select-mode-map
  (let ((map (make-sparse-keymap)))
    (let ((i 0))
      (while (<= i ?\177)
	(define-key map (char-to-string i)
		    'sumibi-kakutei-and-self-insert)
	(setq i (1+ i))))
    (define-key map "\C-m"                   'sumibi-select-kakutei)
    (define-key map "\C-g"                   'sumibi-select-cancel)
    (define-key map "q"                      'sumibi-select-cancel)
    (define-key map "\C-a"                   'sumibi-select-kanji)
    (define-key map "\C-p"                   'sumibi-select-prev)
    (define-key map "\C-n"                   'sumibi-select-next)
    (define-key map sumibi-rK-trans-key      'sumibi-select-next)
    (define-key map (kbd "SPC")              'sumibi-select-next)
    (define-key map "\C-u"                   'sumibi-select-hiragana)
    (define-key map "\C-i"                   'sumibi-select-katakana)
    (define-key map "\C-k"                   'sumibi-select-katakana)
    (define-key map "\C-l"                   'sumibi-select-hankaku)
    (define-key map "\C-e"                   'sumibi-select-zenkaku)
    map)
  "候補選択モードマップ.")
(or (assq 'sumibi-mode minor-mode-map-alist)
    (setq minor-mode-map-alist
          (append (list (cons 'sumibi-mode         sumibi-mode-map)
                        (cons 'sumibi-select-mode  sumibi-select-mode-map))
                  minor-mode-map-alist)))

;; OpenAPIを呼び出さずに固定文字列の結果を返すもの ( copied from GNU Emacs's japanese.el)
(defvar sumibi-japanese-transliteration-rules
  '(( "a" "あ") ( "i" "い") ( "u" "う") ( "e" "え") ( "o" "お")
    ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
    ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
    ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
    ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
    ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
    ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
    ("ya" "や")             ("yu" "ゆ")             ("yo" "よ")
    ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
    ("la" "ら") ("li" "り") ("lu" "る") ("le" "れ") ("lo" "ろ")
    ("wa" "わ") ("wi" "ゐ") ("wu" "う") ("we" "ゑ") ("wo" "を")
    ("n'" "ん") ("nn" "ん")
    ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
    ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
    ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
    ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
    ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")

    ("kya" "きゃ") ("kyu" "きゅ") ("kye" "きぇ") ("kyo" "きょ")
    ("sya" "しゃ") ("syu" "しゅ") ("sye" "しぇ") ("syo" "しょ")
    ("sha" "しゃ") ("shu" "しゅ") ("she" "しぇ") ("sho" "しょ")
    ("cha" "ちゃ") ("chu" "ちゅ") ("che" "ちぇ") ("cho" "ちょ")
    ("tya" "ちゃ") ("tyu" "ちゅ") ("tye" "ちぇ") ("tyo" "ちょ")
    ("nya" "にゃ") ("nyu" "にゅ") ("nye" "にぇ") ("nyo" "にょ")
    ("hya" "ひゃ") ("hyu" "ひゅ") ("hye" "ひぇ") ("hyo" "ひょ")
    ("mya" "みゃ") ("myu" "みゅ") ("mye" "みぇ") ("myo" "みょ")
    ("rya" "りゃ") ("ryu" "りゅ") ("rye" "りぇ") ("ryo" "りょ")
    ("lya" "りゃ") ("lyu" "りゅ") ("lye" "りぇ") ("lyo" "りょ")
    ("gya" "ぎゃ") ("gyu" "ぎゅ") ("gye" "ぎぇ") ("gyo" "ぎょ")
    ("zya" "じゃ") ("zyu" "じゅ") ("zye" "じぇ") ("zyo" "じょ")
    ("jya" "じゃ") ("jyu" "じゅ") ("jye" "じぇ") ("jyo" "じょ")
    ( "ja" "じゃ") ( "ju" "じゅ") ( "je" "じぇ") ( "jo" "じょ")
    ("bya" "びゃ") ("byu" "びゅ") ("bye" "びぇ") ("byo" "びょ")
    ("pya" "ぴゃ") ("pyu" "ぴゅ") ("pye" "ぴぇ") ("pyo" "ぴょ")

    ("kwa" "くゎ") ("kwi" "くぃ") ("kwe" "くぇ") ("kwo" "くぉ")
    ("tsa" "つぁ") ("tsi" "つぃ") ("tse" "つぇ") ("tso" "つぉ")
    ( "fa" "ふぁ") ( "fi" "ふぃ") ( "fe" "ふぇ") ( "fo" "ふぉ")
    ("gwa" "ぐゎ") ("gwi" "ぐぃ") ("gwe" "ぐぇ") ("gwo" "ぐぉ")

    ("dyi" "でぃ") ("dyu" "どぅ") ("dye" "でぇ") ("dyo" "どぉ")
    ("xwi" "うぃ")                  ("xwe" "うぇ") ("xwo" "うぉ")

    ("shi" "し") ("tyi" "てぃ") ("chi" "ち") ("tsu" "つ") ("ji" "じ")
    ("fu"  "ふ")
    ("ye" "いぇ")

    ("va" "ヴぁ") ("vi" "ヴぃ") ("vu" "ヴ") ("ve" "ヴぇ") ("vo" "ヴぉ")

    ("xa"  "ぁ") ("xi"  "ぃ") ("xu"  "ぅ") ("xe"  "ぇ") ("xo"  "ぉ")
    ("xtu" "っ") ("xya" "ゃ") ("xyu" "ゅ") ("xyo" "ょ") ("xwa" "ゎ")
    ("xka" "ヵ") ("xke" "ヶ")

    ("1" "１") ("2" "２") ("3" "３") ("4" "４") ("5" "５")
    ("6" "６") ("7" "７") ("8" "８") ("9" "９") ("0" "０")

    ("!" "！") ("@" "＠") ("#" "＃") ("$" "＄") ("%" "％")
    ("^" "＾") ("&" "＆") ("*" "＊") ("(" "（") (")" "）")
    ("-" "ー") ("=" "＝") ("`" "｀") ("\\" "￥") ("|" "｜")
    ("_" "＿") ("+" "＋") ("~" "￣") ("[" "「") ("]" "」")
    ("{" "｛") ("}" "｝") (":" "：") (";" "；") ("\""  "”")
    ("'" "’") ("." "。") ("," "、") ("<" "＜") (">" "＞")
    ("?" "？") ("/" "／")

    ("z1" "○") ("z!" "●")
    ("z2" "▽") ("z@" "▼")
    ("z3" "△") ("z#" "▲")
    ("z4" "□") ("z$" "■")
    ("z5" "◇") ("z%" "◆")
    ("z6" "☆") ("z^" "★")
    ("z7" "◎") ("z&" "£")
    ("z8" "¢") ("z*" "×")
    ("z9" "♂") ("z(" "【")
    ("z0" "♀") ("z)" "】")
    ("z-" "〜") ("z_" "∴")
    ("z=" "≠") ("z+" "±")
    ("z\\" "＼") ("z|" "‖")
    ("z`" "´") ("z~" "¨")

    ("zq" "《") ("zQ" "〈")
    ("zw" "》") ("zW" "〉")
    ("zr" "々") ("zR" "仝")
    ("zt" "〆") ("zT" "§")
    ("zp" "〒") ("zP" "↑")
    ("z[" "『") ("z{" "〔")
    ("z]" "』") ("z}" "〕")

    ("zs" "ヽ") ("zS" "ヾ")
    ("zd" "ゝ") ("zD" "ゞ")
    ("zf" "〃") ("zF" "→")
    ("zg" "‐") ("zG" "—")
    ("zh" "←")
    ("zj" "↓")
    ("zk" "↑")
    ("zl" "→")
    ("z;" "゛") ("z:" "゜")
    ("z'" "‘") ("z\"" "“")

    ("zx" ":-") ("zX" ":-)")
    ("zc" "〇") ("zC" "℃")
    ("zv" "※") ("zV" "÷")
    ("zb" "°") ("zB" "←")
    ("zn" "′") ("zN" "↓")
    ("zm" "″") ("zM" "〓")
    ("z," "‥") ("z<" "≦")
    ("z." "…") ("z>" "≧")
    ("z/" "・") ("z?" "∞")))

;;;
;;; hooks
;;;
(defvar sumibi-mode-hook nil)
(defvar sumibi-select-mode-hook nil)
(defvar sumibi-select-mode-end-hook nil)

(defconst sumibi-tango-index  0)
(defconst sumibi-annotation-index  1)
(defconst sumibi-kind-index   3)
(defconst sumibi-id-index     4)

;;--- デバッグメッセージ出力
(defvar sumibi-psudo-server nil)         ; クライアント単体で仮想的にサーバーに接続しているようにしてテストするモード

;;--- デバッグメッセージ出力
(defvar sumibi-debug nil)        ; デバッグフラグ
(defun sumibi-debug-print (string)
  "引数STRINGで指定されたデバッグメッセージを*sumibi-debug* に出力する."
  (if sumibi-debug
      (let
          ((buffer (get-buffer-create "*sumibi-debug*")))
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert string)))))

(defun sumibi-debug-save-dashboard ()
  "デバッグ情報を~/.sumibi/sumibi-debug-dashboard.htmlに保存する."
  (interactive)
  (when sumibi-debug  
    (let ((filename (expand-file-name "~/.sumibi/sumibi-debug-dashboard.html")))
      ;; ディレクトリが存在しない場合は作成
      (let ((dir (file-name-directory filename)))
        (unless (file-directory-p dir)
          (make-directory dir t)))
      (with-temp-buffer
	(insert "<!DOCTYPE html>\n")
	(insert "<html>\n")
	(insert "<head>\n")
	(insert "<meta charset=\"UTF-8\">\n")
	(insert "<title>Sumibi Debug Dashboard</title>\n")
	(insert "<style>\n")
	(insert "body { font-family: monospace; margin: 20px; }\n")
	(insert "h1 { color: #333; }\n")
	(insert "h2 { color: #666; margin-top: 30px; }\n")
	(insert "table { border-collapse: collapse; width: 100%; margin-top: 10px; }\n")
	(insert "th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }\n")
	(insert "th { background-color: #f2f2f2; }\n")
	(insert "tr:nth-child(even) { background-color: #f9f9f9; }\n")
	(insert ".variable { background-color: #e8f4f8; padding: 5px; margin: 5px 0; }\n")
	(insert "</style>\n")
	(insert "</head>\n")
	(insert "<body>\n")
	(insert "<h1>Sumibi Debug Dashboard</h1>\n")
	(insert "<p>Generated at: " (current-time-string) "</p>\n")
	
	;; カスタム変数の値を出力
	(insert "<h2>Custom Variables</h2>\n")
	(let ((custom-vars '(sumibi-stop-chars
                             sumibi-mozc-learn-at-kakutei
                             sumibi-backend
                             sumibi-current-model
                             sumibi-model-list
                             sumibi-history-stack-limit
                             sumibi-api-timeout
                             sumibi-threshold-letters-of-long-sentence
                             sumibi-surrounding-lines)))
          (dolist (var custom-vars)
            (insert (format "<div class=\"variable\"><strong>%s:</strong> %s</div>\n"
                            (symbol-name var)
                            (html-escape-string (format "%S" (symbol-value var)))))))
	
	;; sumibi-history-stackをテーブルで出力
	(insert "<h2>History Stack</h2>\n")
	(insert (format "<p>Total entries: %d</p>\n" (length sumibi-history-stack)))
	
	;; デバッグ情報を追加
	(insert "<h3>Raw Stack Data</h3>\n")
	(insert (format "<pre>%s</pre>\n" (html-escape-string (format "%S" sumibi-history-stack))))
	
	(if sumibi-history-stack
            (progn
              (insert "<table>\n")
              (insert "<tr><th>Index</th><th>Buffer Name</th><th>Markers</th><th>Genbun</th><th>Last-Fix</th><th>Cand-Cur</th><th>Cand-Len</th><th>Henkan-Kouho-List</th></tr>\n")
              (let ((index 0))
		(dolist (entry sumibi-history-stack)
		  (let ((bufname (cdr (assoc 'bufname entry)))
			(markers (cdr (assoc 'markers entry)))
			(genbun (cdr (assoc 'genbun entry)))
			(last-fix (cdr (assoc 'last-fix entry)))
			(cand-cur (cdr (assoc 'cand-cur entry)))
			(cand-len (cdr (assoc 'cand-len entry)))
			(henkan-kouho-list (cdr (assoc 'henkan-kouho-list entry))))
                    (insert "<tr>\n")
		    (insert (format "<td>%d</td>\n" index))
		    (insert (format "<td>%s</td>\n" (html-escape-string (or bufname "N/A"))))
		    (insert (format "<td>%s</td>\n" (html-escape-string markers)))
		    (insert (format "<td>%s</td>\n" (html-escape-string (or genbun "N/A"))))
		    (insert (format "<td>%s</td>\n" (html-escape-string (or last-fix "N/A"))))
		    (insert (format "<td>%s</td>\n" (html-escape-string (format "%S" cand-cur))))
		    (insert (format "<td>%s</td>\n" (html-escape-string (format "%S" cand-len))))
		    (insert (format "<td>%s</td>\n" (html-escape-string (format "%S" henkan-kouho-list))))
		    (insert "</tr>\n")
		    (setq index (1+ index)))))
	      (insert "</table>\n"))
	  (insert "<p>No history entries found.</p>\n"))
	
	(insert "</body>\n")
	(insert "</html>\n")
	(write-file filename)
	(message "Debug dashboard saved to %s" filename)))))

(defun html-escape-string (string)
  "HTMLエスケープ処理を行う."
  (let ((str (cond
	      ((stringp string) 
	       ;; 文字列の場合は、HTMLエスケープを最小限に抑える
	       (replace-regexp-in-string
                "&" "&amp;"
                string))
	      ((markerp string) 
	       (if (marker-position string)
                   (format "marker:%d" (marker-position string))
                 "marker:deleted"))
	      ((consp string)
	       (cond
                ;; マーカーのペア
                ((and (markerp (car string)) (markerp (cdr string)))
                 (let ((start-pos (marker-position (car string)))
		       (end-pos (marker-position (cdr string))))
                   (format "(%s . %s)" 
                           (if start-pos (number-to-string start-pos) "deleted")
                           (if end-pos (number-to-string end-pos) "deleted"))))
                ;; 通常のcons
                (t (let ((raw-str (prin1-to-string string)))
                     (replace-regexp-in-string
		      "#<\\([^>]*\\)>"
		      "[\\1]"
		      raw-str)))))
	      (t 
	       ;; 通常のオブジェクトの場合、文字列化してから問題のある文字を置換
	       (let ((raw-str (prin1-to-string string)))
                 (replace-regexp-in-string
                  "#<\\([^>]*\\)>"
                  "[\\1]"
                  raw-str))))))
    ;; HTMLエスケープ処理（文字列以外の場合のみ）
    (if (stringp string)
        str  ; 文字列の場合は既に処理済み
      (replace-regexp-in-string
       "&" "&amp;"
       (replace-regexp-in-string
        "<" "&lt;"
        (replace-regexp-in-string
         ">" "&gt;"
         str))))))

;; HTTPクライアントの多重起動防止用
(defvar sumibi-busy 0)


;;; 候補選択モード用
(defvar sumibi-history-stack '())        ; 過去に変換した、場所と変換候補の状態を保存しておくスタック
(defvar sumibi-history-file-path "~/.sumibi/history.jsonl") ; 履歴ファイルのパス（テスト時に変更可能）
;; データ構造は以下の通り。
;; alistのlistとなる。 alistのキーは、sumibi-* というバッファローカル変数のバックアップとなる)
;; 新しいものは先頭に追加され、検索も先頭から行われる。即ち、古い情報も残るがいつかstackのlimitを超えるとあふれて捨てられる。
;;(
;; (
;;  (bufname           . "*scratch*")
;;  (markers           . '(#<marker at 192 in *scratch*> . #<marker at 194 in *scratch*>))
;;  (cand-cur          . 0)
;;  (cand-cur-backup   . 0)
;;  (cand-len          . 0)
;;  (last-fix          . 0)
;;  (henkan-kouho-list . '())
;;  ))
(defvar sumibi-fence-start nil)          ; fence 始端marker
(defvar sumibi-fence-end nil)            ; fence 終端marker
(defvar sumibi-henkan-separeter " ")     ; fence mode separeter
(defvar sumibi-cand-cur 0)               ; カレント候補番号
(defvar sumibi-cand-cur-backup 0)        ; カレント候補番号(UNDO用に退避する変数)
(defvar sumibi-cand-len nil)             ; 候補数
(defvar sumibi-last-fix "")              ; 最後に確定した文字列
(defvar sumibi-last-roman "")            ; 最後にsumibi-serverにリクエストしたローマ字文字列
(defvar sumibi-genbun "")                ; 原文（変換前の文字列）
(defvar sumibi-select-operation-times 0) ; 選択操作回数
(defvar sumibi-henkan-kouho-list nil)    ; 変換結果リスト(サーバから帰ってきたデータそのもの)


;; その他
(defvar sumibi-markers '())              ; 単語の開始、終了位置のpair。 次のような形式で保存する ( 1 . 2 )
(defvar sumibi-timer    nil)             ; インターバルタイマー型変数
(defvar sumibi-timer-rest  0)            ; あと何回呼出されたら、インターバルタイマの呼出を止めるか
(defvar sumibi-last-lineno 0)            ; 最後に変換を実行した行番号
(defvar sumibi-guide-overlay   nil)      ; リアルタイムガイドに使用するオーバーレイ
(defvar sumibi-last-request-time 0)      ; OpenAI 互換サーバーへ最後にリクエストした時刻 (秒)
(defvar sumibi-guide-lastquery  "")      ; OpenAI 互換サーバーへ最後に送ったクエリ文字列
(defvar sumibi-guide-lastresult '())     ; OpenAI 互換サーバーから最後に受け取った結果



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ユーティリティ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sumibi-assoc-ref (key alist fallback)
  "指定したassocから内容を取り出すユーティリティ.
Argument KEY: key of alist.
Argument ALIST: alist.
Argument FALLBACK: fallback function."
  (let ((entry (assoc key alist)))
    (if entry
        (cdr entry)
      fallback)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 先頭プレフィックス保持ユーティリティ
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sumibi--split-markdown-prefix (str)
  "Return cons cell (PREFIX . BODY) by separating STR into a markdown-related
prefix (leading spaces, list markers, or heading hashes) and the rest.

The PREFIX part is kept as-is and must not be fed to the conversion
engine so that constructs like `- ', `# ', or indent spaces are
preserved. BODY is the portion that should be converted."
  (cond
   ;; Markdown heading (with or without trailing space)
   ((string-match "\`[ \t]*#+[ \t]*" str)
    (let* ((m (match-string 0 str))
           (prefix (if (string-suffix-p " " m) m (concat m " "))))
      (cons prefix (substring str (match-end 0)))))
   ;; Markdown list marker '-' or '*' (with or without trailing space)
   ((string-match "\`[ \t]*[-*][ \t]*" str)
    (let* ((m (match-string 0 str))
           (prefix (if (string-suffix-p " " m) m (concat m " "))))
      (cons prefix (substring str (match-end 0)))))
   ;; Pure leading whitespace (code block indent etc.)
   ((string-match "\`[ \t]+" str)
    (cons (match-string 0 str) (substring str (match-end 0))))
   (t
    (cons "" str))))

;; Ensure a single space after Markdown heading (#...) or list marker (-,*)
(defun sumibi--ensure-space-after-heading (pos)
  "Insert a single space after Markdown marker if missing.

POS is the buffer position where converted text starts.  If the
character immediately before POS is one of `#', `-' or `*' *and* we
are at the beginning of a Markdown heading or list item (that is,
only whitespace precedes the marker on that line), ensure there is a
space between the marker and the text.  This prevents constructs like
`*項目' or `###見出し' that break Markdown syntax."
  (when (> pos (point-min))
    (save-excursion
      (goto-char pos)
      (let* ((marker (char-before))
             (marker? (memq marker '(?# ?- ?*))))
        (when marker?
          ;; Verify marker is at list/heading position.
          (let ((bol (line-beginning-position))
                (p (1- (point))))
            ;; Skip backward over additional marker chars for headings (#).
            (when (eq marker ?#)
	      (while (and (> p bol) (eq (char-before p) ?#))
                (setq p (1- p))))
            ;; Skip backward over whitespace.
            (while (and (> p bol) (memq (char-before p) '(?  ?\t)))
	      (setq p (1- p)))
            (when (= p bol)
	      ;; Confirm there's no space after the marker(s).
	      (unless (memq (char-after) '(?  ?\t))
                (insert " ")))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 表示系関数群
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sumibi-use-fence t)
(defvar sumibi-use-color nil)
(defvar sumibi-init nil)

;;
;; 初期化
;;
(defun sumibi-init ()
  "Sumibi環境の初期化を行う."
  (if sumibi-init
      t
    (cond
     ((and (not (sumibi-backend-mozc-p))
	   (not (getenv "SUMIBI_AI_API_KEY"))
           (not (getenv "OPENAI_API_KEY")))
      (message "%s" "Please set SUMIBI_AI_API_KEY or OPENAI_API_KEY environment variable."))
     ((and (>= emacs-major-version 28) (>= emacs-minor-version 1))
      ;; 履歴ファイルから履歴を読み込む
      (sumibi-load-history-from-file)
      ;; 初期化完了
      (setq sumibi-init t))
     (t
      (message "%s" "Emacs version 28.1 or higher is required.")))))

(defun sumibi-escape-for-json (str)
  "引数STRで指定した、JSON文字列に含まれるバックスペース、ダブルクォーテーション、改行、タブをエスケープする."
  (let* ((str1 (string-replace "\\" "" str))
         (str2 (string-replace "\"" "\\\"" str1))
         (str3 (string-replace "\n" "\\n" str2))
         (str4 (string-replace "	" "\\t" str3))
         (str5 (unicode-escape str4)))
    str5))

(defun sumibi-parse-http-body (buf)
  "Pickup http status and body string from buf string.
Argument BUF : http response buffer"
  (with-current-buffer buf
    (decode-coding-string
     (let ((str (buffer-substring-no-properties (point-min) (point-max))))
       (sumibi-debug-print (format "<<<%s>>>\n" str))
       str)
     'utf-8)
    (goto-char (point-min))
    (let ((status-line (buffer-substring (point-min) (progn (end-of-line) (point)))))
      ;; Extract status code
      (sumibi-debug-print (format "status-line: [%s]\n" status-line))
      (let ((status-code (if (string-match "^HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
                             (match-string 1 status-line)
                           "400")))
        (sumibi-debug-print (format "status-code: [%s]\n" status-code))
        (re-search-forward "^$")
        (forward-char)
        (let ((body (buffer-substring (point) (point-max))))
          ;; Return appropriate response based on status code
          (if (string= status-code "200")
              (cons status-code body)
            ;; For non-200, return structured error message
            (cond
             ((string= status-code "401")
              (cons status-code "{\"error\": { \"message\" : \"認証エラー: APIキーが無効または期限切れです\"}}"))
             ((string= status-code "403")
              (cons status-code "{\"error\": { \"message\" : \"認可エラー: このAPIキーにはアクセス権限がありません\"}}"))
             (t
              (cons status-code "{\"error\": { \"message\" : \"HTTPエラーが発生しました\"}}")))))))))

;;
;; OpenAI 互換 API にプロンプトを発行する
;;
(defun sumibi-openai-http-post (message-lst
                                arg-n
                                sync-func
                                deferred-func
                                deferred-func2)
  "OpenAI 互換 ChatCompletions API を呼び出します。
Argument MESSAGE-LST : ChatCompletions API に渡す role と content のリスト。
Argument ARG-N : ChatCompletions API の引数 n の値。
Argument SYNC-FUNC : 同期呼び出し時のコールバック関数。非同期呼び出しの場合は nil を指定します。
Argument DEFERRED-FUNC : 非同期呼び出し時のコールバック関数 (1)。
Argument DEFERRED-FUNC2 : 非同期呼び出し時のコールバック関数 (2)。"
  (sumibi-debug-print (format "sumibi-openai-http-post()\n"))
  (let* ((base (sumibi-ai-base-url))
         (url (concat base "/chat/completions")))
    (setq url-request-method "POST")
    (setq url-http-version "1.1")
    (setq url-request-extra-headers
          `(("Content-Type" . "application/json; charset=utf-8")
            ("Authorization" . ,(concat "Bearer " (or (getenv "SUMIBI_AI_API_KEY") (getenv "OPENAI_API_KEY"))))))
    (setq url-request-data
          (concat
           "{"
           (format "  \"model\": \"%s\"," (sumibi-ai-model))
           (if (sumibi-gpt5-series-p)
               "  \"temperature\": 1.0,"
             "  \"temperature\": 0.8,")
           (format  "  \"n\": %d," arg-n)
           (if (sumibi-gpt5-series-p)
               "  \"reasoning_effort\": \"minimal\",  \"verbosity\": \"low\","
             "")
           "  \"messages\": [ "
           (string-join
            (-map
             (lambda (x)
	       (format " {\"role\": \"%s\",    \"content\": \"%s\"}"
		       (car x)
		       (sumibi-escape-for-json (cdr x))))
             message-lst)
            ",")
           "  ] "
           "}"))
    (cond
     ((not deferred-func2) ;; 同期バージョン
      (let ((status-and-body
             (let ((buf (url-retrieve-synchronously url t t sumibi-api-timeout)))
	       (sumibi-debug-print (buffer-name buf))
	       (sumibi-debug-print "\n")
	       (if buf
		   (sumibi-parse-http-body buf)
                 (cons "504" "{\"error\": { \"message\" : \"TIMEOUT ERROR\"}}\n")))))
        (funcall sync-func (cdr status-and-body))))

     (t ;; 非同期バージョン
      (deferred:$
       (deferred:url-retrieve url)
       (deferred:nextc it
		       (lambda (buf)
			 (sumibi-debug-print (buffer-name buf))
			 (sumibi-debug-print "\n")
			 (if buf
			     (sumibi-parse-http-body buf)
			   (cons "504" "{\"error\": { \"message\" : \"TIMEOUT ERROR\"}}\n"))))
       (deferred:nextc it
		       (lambda (status-and-body)
			 (atomic-change-group
			   (funcall deferred-func2)
			   (sumibi-debug-print (format "<<<%s>>>\n" (funcall deferred-func (cdr status-and-body)))))
			 t))))
     '())))



(defun sumibi-analyze-openai-json-obj (json-obj arg-n)
  "JSONから変換結果の文字列を取り出す.
引数JSON-OBJ: パース済みのJSONオブジェクト
引数ARG-N: 候補を何件返すか"
  (let ((result '())
        (count 0))
    (cond
     ((gethash "error" json-obj)
      (let ((obj (gethash "error" json-obj)))
	(list (concat "!!" (gethash "message" obj) "!!"))))
     (t
      (while (< count arg-n)
        (let* ((hex-str
                (gethash "content"
                         (gethash "message"
                                  (aref (gethash "choices" json-obj) count))))
	       (utf8-str
                (decode-coding-string (url-unhex-string hex-str) 'utf-8))
	       (clean-str
		;; 末尾の改行文字を削除する
		(string-trim-right utf8-str "\n")))
          (setq result (cons clean-str result)))
        (setq count (1+ count)))
      result))))

(defun sumibi-roman-to-kanji-with-surrounding (roman surrounding arg-n deferred-func2)
  "ローマ字で書かれた文章を **OpenAI 互換** サーバーを使って変換し、
結果を文字列で返します。変換対象の文章の周辺の文章も受け取ります。

本関数は行頭のインデントや Markdown のリスト記号 (\"- \", \"* \")、
見出し記号 (\"# \", \"## \" など) を *変換対象から除外* して保持します。

ROMAN: 変換対象のローマ字文字列 (行頭のプレフィックス込み)
SURROUNDING: 変換対象周辺の文章
ARG-N: 候補を何件返すか
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2).
戻り値: (\"1番目の文章の文字列\" \"2番目の文章の文字列\" \"3番目の文章の文字列\" ...)"
  ;; プレフィックスを抽出して保持 ----------------------------------
  (sumibi-debug-print (format "sumibi-roman-to-kanji-with-surrounding()\n"))
  (let* ((split (sumibi--split-markdown-prefix roman))
         (prefix (car split))
         (core-roman (cdr split)))
    ;; `mozc' backend -------------------------------------------------
    (if (sumibi-backend-mozc-p)
        (let ((cands (sumibi-mozc--candidate-list core-roman arg-n)))
          (mapcar (lambda (s)
                    (let ((ret (concat prefix s)))
		      (when (get-text-property 0 'sumibi-mozc-candidate s)
                        (put-text-property 0 (length ret) 'sumibi-mozc-candidate t ret))
		      ret))
                  cands))
      ;; default: OpenAI backend -------------------------------------
      (let ((saved-marker (point-marker))
            (result nil))
        (sumibi-openai-http-post
         (list
	  (cons "system"
		(concat
		 "あなたはローマ字とひらがなを日本語に変換するアシスタントです。"
		 "ローマ字の 「nn」 は 「ん」と読んでください。"
		 "[](URL)のようなmarkdown構文は維持してください。"
		 "# や ## や ### や #### のようなmarkdown構文は維持してください。"
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 "ローマ字の字面をそのままひらがなや漢字にするだけで、元のローマ字にない文章を作り出さないでください。"
		 "出力は変換後の一文のみ。注釈や説明は一切付けないください。"
		 "もし、入力された文章が英語の文章と判断できた場合は、日本語に翻訳してください。"))
	  (cons "user"
		(concat
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 " 周辺の文章は、「こんにちは、中野です。watashi no namae ha nakano desu . どうぞよろしくお願いします。」"
		 "のような文章になっています。"
		 "周辺の文脈を見てそれに合った語彙を選んでください。: watashi no namae ha nakano desu ."))
	  (cons "assistant"
		"私の名前は中野です。")
	  (cons "user"
		(concat
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 "周辺の文章は、「説明はここまでです。それ以外はikano toori desu .」"
		 "のような文章になっています。"
		 "周辺の文脈を見てそれに合った語彙を選んでください。: ikano toori desu ."))
	  (cons "assistant"
		"以下の通りです。")
	  (cons "user"
		(concat
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 "周辺の文章は、「開始位置から終了位置までをhannishitei shimasuそれでは続いて、」"
		 "のような文章になっています。"
		 "周辺の文脈を見てそれに合った語彙を選んでください。: hannishitei shimasu"))
	  (cons "assistant"
		"範囲指定します")
	  (cons "user"
		(concat
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 "周辺の文章は、「見てください!We succeeded in taking a photo like this:\n![example](https://www.example.com/dir1/dir2/example.png)、"
		 "リアルな写真だと思いませんか？」"
		 "のような文章になっています。"
		 "周辺の文脈を見てそれに合った語彙を選んでください。: We succeeded in taking a photo like this:\n![example](https://www.example.com/dir1/dir2/example.png)"))
	  (cons "assistant"
		"このような写真を撮ることに成功しました：\n![例](https://www.example.com/dir1/dir2/example.png)")
	  (cons "user"
		(concat
		 "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		 "周辺の文章は、「ここまでが前半の説明です。\n"
		 "## this is markdown section\n"
		 "\n"
		 "」"
		 "のような文章になっています。"
		 "周辺の文脈を見てそれに合った語彙を選んでください。: ## this is markdown section"))
	  (cons "assistant"
		"## これはMarkdownのセクションです。")
	  (cons "user"
		(format
		 (concat 
		  "ローマ字とひらがなの文を漢字仮名混じり文にしてください。"
		  "周辺の文章は、「%s」"
		  "のような文章になっています。"
		  "周辺の文脈を見てそれに合った語彙を選んでください。: %s")
		 surrounding core-roman)))
	 arg-n
	 (lambda (json-str)
	   (let* ((json-obj (json-parse-string json-str))
                  (lst (sumibi-analyze-openai-json-obj json-obj arg-n)))
             (setq result (mapcar (lambda (s) (concat prefix s)) lst))))
	 (lambda (json-str)
	   (let* ((json-obj (json-parse-string json-str))
		  (lst (mapcar (lambda (s) (concat prefix s))
			       (sumibi-analyze-openai-json-obj json-obj arg-n))))
             (when (and lst (null deferred-func2))
	       (setq result lst))
             (when lst
	       (save-excursion
		 (goto-char (marker-position saved-marker))
		 (insert (car lst))
		 ;; 見出し `###` 等の直後にスペースが無ければ補完する
		 (sumibi--ensure-space-after-heading (marker-position saved-marker))
		 (goto-char (marker-position saved-marker))))))
	 deferred-func2)
        result))))

(defun sumibi-roman-to-yomigana (roman deferred-func2)
  "ローマ字で書かれた文章を **OpenAI 互換** サーバーを使って読み仮名を返します。
ROMAN: \"shita\" や \"nano\"
ARG-N: 候補を何件返すか
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2).
戻り値: (\"した\" \"シタ\") や (\"なの\" \"ナノ\")"
  (sumibi-debug-print (format "sumibi-roman-to-yomigana()\n"))
  (if (sumibi-backend-mozc-p)
      '()
    (let ((saved-marker (point-marker)))
      (sumibi-openai-http-post
       (list
	(cons "system"
	      "あなたはローマ字をひらがなとカタカナに変換するアシスタントです。ローマ字の 「nn」 は 「ん」と読んでください。")
	(cons "user"
	      "ローマ字をひらがなとカタカナにしてください : shita")
	(cons "assistant"
	      "した シタ")
	(cons "user"
	      "ローマ字をひらがなとカタカナにしてください : nano")
	(cons "assistant"
	      "なの ナノ")
	(cons "user"
	      "ローマ字をひらがなとカタカナにしてください : aiueokakikukeko")
	(cons "assistant"
	      "あいうえおかきくけこ アイウエオカキクケコ")
	(cons "user"
	      (format "ローマ字をひらがなとカタカナにしてください : %s" roman)))
       1
       (lambda (json-str)
	 (let ((json-obj (json-parse-string json-str)))
           (split-string (car (sumibi-analyze-openai-json-obj json-obj 1)))))
       (lambda (json-str)
	 (let* ((json-obj (json-parse-string json-str))
		(lst (split-string (car (sumibi-analyze-openai-json-obj json-obj 1)))))
           (if lst
	       (save-excursion
		 (goto-char (marker-position saved-marker))
		 (insert (car lst))
		 (goto-char (marker-position saved-marker))))))
       deferred-func2))))

(defun sumibi-kanji-to-yomigana (kanji deferred-func2)
  "漢字仮名混じりで書かれた文章を **OpenAI 互換** サーバーを使って読み仮名を返します。
KANJI: \"日本語\" のような文字列
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2).
戻り値: (\"にほんご\" \"ニホンゴ\")"
  (sumibi-debug-print (format "sumibi-kanji-to-yomigana()\n"))
  (let ((saved-marker (point-marker)))
    (sumibi-openai-http-post
     (list
      (cons "system"
            "あなたは漢字が与えられると、ひらがなとカタカナとその漢字の同音異義語を返すアシスタントです。")
      (cons "user"
            "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : 東西南北")
      (cons "assistant"
            "とうざいなんぼく トウザイナンボク 東西南北")
      (cons "user"
            "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : 漢字")
      (cons "assistant"
            "かんじ カンジ 漢字 感じ 幹事 監事 寛二")
      (cons "user"
            (format "ひらがなとカタカナと同音異義語をなるべく多く列挙してください。 : %s" kanji)))
     1
     (lambda (json-str)
       (let ((json-obj (json-parse-string json-str)))
         (split-string (car (sumibi-analyze-openai-json-obj json-obj 1)))))
     (lambda (json-str)
       (let* ((json-obj (json-parse-string json-str))
	      (lst (split-string (car (sumibi-analyze-openai-json-obj json-obj 1)))))
         (if lst
             (save-excursion
	       (goto-char (marker-position saved-marker))
	       (insert (car lst))
	       (goto-char (marker-position saved-marker))))))
     deferred-func2)))

(defun sumibi-kanji-to-english (kanji arg-n deferred-func2)
  "日本語の文章を、**OpenAI 互換** サーバーを使って英語に翻訳します。
KANJI: \"私の名前は中野です。\" のような文字列
ARG-N: 候補を何件返すか
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2).
戻り値: (\"My name is Nakano.\" \"My name is Nakano.\" \"My name is Nakano.\")"
  (sumibi-debug-print (format "sumibi-kanji-to-english()\n"))
  (let ((saved-marker (point-marker)))
    (sumibi-openai-http-post
     (list
      (cons "system"
            "あなたは、与えられた文章を英語に翻訳するアシスタントです。")
      (cons "user"
            "文章を英語に翻訳してください。 : 私の名前は中野です。")
      (cons "assistant"
            "My name is Nakano.")
      (cons "user"
            "文章を英語に翻訳してください。 : GPTはOpenAIから2018年に以下の論文で提案されたモデルで、基本的にはTransformerをベースに、事前学習-ファインチューニングをすることで非常に高い精度を達成したモデルです。")
      (cons "assistant"
            "GPT is a model proposed by OpenAI in 2018 in the following paper, which is basically based on Transformer and achieves very high accuracy by pre-training - fine tuning.")
      (cons "user"
            (format "文章を英語に翻訳してください。 : %s" kanji)))
     arg-n
     (lambda (json-str)
       (let ((json-obj (json-parse-string json-str)))
         (sumibi-analyze-openai-json-obj json-obj arg-n)))
     (lambda (json-str)
       (let* ((json-obj (json-parse-string json-str))
	      (lst (sumibi-analyze-openai-json-obj json-obj arg-n)))
         (if lst
             (save-excursion
	       (goto-char (marker-position saved-marker))
	       (insert (car lst))
	       (goto-char (marker-position saved-marker))))))
     deferred-func2)))

(defun sumibi-determine-number-of-n (request-str)
  "引数 REQUEST-STR から ChatCompletions API の引数「n」に指定する数を決める。"
  (if (string= (sumibi-ai-base-url) "https://api.openai.com")
      (if (<= sumibi-threshold-letters-of-long-sentence (length request-str))
	  1
	3)
    1))

(defun sumibi-determine-sync-p (request-str)
  "引数 REQUEST-STR から ChatCompletions API を非同期で呼び出すかを決める。"
  (> sumibi-threshold-letters-of-long-sentence (length request-str)))


(defun sumibi-inverse-henkan (roman arg-n deferred-func2)
  "日本語=>英語翻訳を実行する.
ROMAN: \"私の名前は中野です。\" のような文字列
ARG-N: 候補を何件返すか
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2)."
  (let ((lst (sumibi-kanji-to-english roman arg-n deferred-func2)))
    (append
     (-map
      (lambda (x)
        (list (car x)
	      (sumibi--annotation-label (car x) (+ 1 (cdr x)))
	      0 'l (cdr x)))
      (-zip-pair
       lst
       '(0 1 2 3 4)))
     (list
      (list roman "原文まま" 0 'l (length lst))))))


(defun sumibi-supplement-kouho (kouho-lst)
  "/ro-karu jisyo を使って、変換候補をおぎなう.
KOUHO-LST: (\"にほんご\" \"ニホンゴ\") のようなリスト.
必ずしも先頭にひらがな候補が入っているとは限らない
"
  ;; ひらがな候補を探す
  (let ((hiragana-kouho-lst
	 (-filter
	  (lambda (str)
	    (string-match-p "^[ぁ-ん]+$" str))
	  kouho-lst)))
    (if hiragana-kouho-lst
	(let* ((hiragana-kouho
		(car hiragana-kouho-lst))
	       (extra-kouho-lst
		(-filter
		 (lambda (x)
		   (string= hiragana-kouho (car x)))
		 sumibi-localdic)))
	  (if extra-kouho-lst
	      (append kouho-lst (car (cdr (car extra-kouho-lst))))
	    kouho-lst))
      kouho-lst)))


(defun sumibi-nihongo-saihenkan (roman deferred-func2)
  "日本語を再変換する.
ROMAN: \"日本語\" のような変換済の文字列
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2)."
  (let* ((lst (sumibi-kanji-to-yomigana roman deferred-func2))
	 (extended-lst 
          (sumibi-supplement-kouho lst))
	 (kouho-lst
          (-map
           (lambda (x)
             (list (car x)
                   (sumibi--annotation-label (car x) (+ 1 (cdr x)))
                   0 
                   (sumibi-determine-candidate-type (car x))
                   (cdr x)))
           (-zip-pair
	    extended-lst
	    '(
	      0 1 2 3 4 5 6 7 8 9
	      10 11 12 13 14 15 16 17 18 19
	      20 21 22 23 24 25 26 27 28 29
	      30 31 32 33 34 35 36 37 38 39
	      40 41 42 43 44 45 46 47 48 49
	      )))))
    (append
     kouho-lst
     (list (list roman "原文まま" 0 'l (length kouho-lst))))))

(defun sumibi-alphabet-henkan (roman surrounding-text arg-n deferred-func2)
  "アルファベット(ローマ字or英語の文章)からカナ漢字混じり文へ変換する.
ROMAN: \"watashi no namae ha nakano desu\" のような文字列
ARG-N: 候補を何件返すか
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2)."
  (let ((lst (sumibi-roman-to-kanji-with-surrounding roman surrounding-text arg-n deferred-func2)))
    (when (>= 10 (length roman))
      (setq lst
            (append
             lst
             (sumibi-roman-to-yomigana roman deferred-func2))))
    (setq
     lst
     (sumibi-supplement-kouho lst))
    (append
     (-map
      (lambda (x)
        (list (car x)
	      (sumibi--annotation-label (car x) (+ 1 (cdr x)))
	      0 
	      (sumibi-determine-candidate-type (car x))
	      (cdr x)))
      (-zip-pair
       lst
       '(
	 0 1 2 3 4 5 6 7 8 9
	 10 11 12 13 14 15 16 17 18 19
	 20 21 22 23 24 25 26 27 28 29
	 30 31 32 33 34 35 36 37 38 39
	 40 41 42 43 44 45 46 47 48 49)))
     (list
      (list roman "原文まま" 0 'l (length lst))))))

(defun sumibi-hiragana-to-katakana (str)
  "ひらがな文字列をカタカナに変換して返す。もし、ひらがな以外の場合はnilを返す.
str: ひらがな文字列"
  (if (string-match-p "[^ぁ-ん]" str)
      nil
    (apply 'concat
           (mapcar (lambda (char)
                     (if (and (>= char #x3041) (<= char #x3096))
                         (string (+ char #x60))
		       (string char)))
                   (string-to-list str)))))

(defun sumibi-katakana-to-hiragana (str)
  "カタカナ文字列STRをひらがなに変換して返す。カタカナ以外の文字はそのまま返す。"
  (apply 'concat
         (mapcar (lambda (char)
                   (if (and (>= char #x30A1) (<= char #x30F6))
		       (string (- char #x60))
                     (string char)))
                 (string-to-list str))))

(defun sumibi-determine-candidate-type (str)
  "候補文字列STRから適切な候補タイプを判定する.
戻り値: 'j (漢字), 'h (ひらがな), 'k (カタカナ), 'l (半角), 'z (全角)"
  (cond
   ;; ひらがなのみ
   ((string-match-p "^[ぁ-ん]+$" str) 'h)
   ;; カタカナのみ
   ((string-match-p "^[ァ-ヶ]+$" str) 'k)
   ;; 漢字を含む
   ((sumibi-string-include-kanji str) 'j)
   ;; 半角文字のみ
   ((string-match-p "^[[:ascii:]]+$" str) 'l)
   ;; その他（全角）
   (t 'z)))

(defun sumibi-mozc-candidates-to-structure (cands)
  "Mozc候補リストを適切な候補構造に変換する.
CANDS: Mozcから返された候補文字列のリスト
戻り値: 構造化された候補リスト"
  (-map
   (lambda (x)
     (list (car x)
           (if (get-text-property 0 'sumibi-mozc-candidate (car x))
               (sumibi--annotation-label (car x) (+ 1 (cdr x)))
             (sumibi--annotation-label (car x) (+ 1 (cdr x))))
           0
           (sumibi-determine-candidate-type (car x))
           (cdr x)))
   (-zip-pair
    cands
    (-iota (length cands)))))

(defun sumibi-henkan-request (roman surrounding-text inverse-flag deferred-func2)
  "ローマ字で書かれた文章を複数候補作成して返す.
ROMAN: \"watashi no namae ha nakano desu\" のような文字列
INVERSE-FLAG: 英語から日本語への逆変換の場合だけ t を指定する
DEFERRED-FUNC2: 非同期呼び出し時のコールバック関数(2)."
  (let ((fixed-kouho
         (-filter
          (lambda (x)
            (string= roman (car x)))
          sumibi-japanese-transliteration-rules)))
    (cond
     (inverse-flag
      (sumibi-inverse-henkan roman (sumibi-determine-number-of-n roman) deferred-func2))
     (t
      (cond
       ;; 固定の変換キーワードの場合(wo ha ga...)
       ((< 0 (length fixed-kouho))
	(let* ((fixed-str
		(cadr (car fixed-kouho)))
	       (fixed-katakana
		(sumibi-hiragana-to-katakana fixed-str)))
	  (if fixed-katakana
	      (list
	       (list fixed-str "固定文字列" 0 'j 0)
	       (list fixed-katakana "固定文字列" 0 'j 0))
	    (list
	     (list fixed-str "固定文字列" 0 'j 0)))))
       ;; 漢字を含む場合
       ((sumibi-string-include-kanji roman)
        (sumibi-nihongo-saihenkan roman deferred-func2))
       (t
        (sumibi-alphabet-henkan roman surrounding-text (sumibi-determine-number-of-n roman) deferred-func2)))))))


(defun sumibi-file-existp (file)
  "FILE が存在するかどうかをチェックする。 t か nil で結果を返す."
  (let* ((file (or (car-safe file)
                   file))
         (file (expand-file-name file)))
    (file-exists-p file)))


(defun sumibi-extract-lines-around-point (p n)
  "ポイント P を中心に、前後 N 行、合計 (2N+1) 行のテキストを取り出す.バッファ先頭・末尾に達してもエラーにならない.
P: \"中心位置P
n: \"N行
戻り値: ポイント P の前後を取り出した文字列"
  (save-excursion
    (goto-char p)
    (beginning-of-line)
    ;; 前にN行移動（バッファ先頭超えないように）
    (let ((start (point)))
      (forward-line (- n))
      (setq start (point)) ;; 実際に移動した位置を記録
      ;; 中心に戻って、後ろにN行進む
      (goto-char p)
      (beginning-of-line)
      (forward-line n)
      (end-of-line)
      (let ((end (point)))
        (buffer-substring-no-properties start end)))))


(defun sumibi-henkan-region-sync (b e inverse-flag)
  "リージョンをローマ字漢字変換する(同期関数バージョン).
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument INVERSE-FLAG：逆変換かどうか"
  (when (/= b e)
    (let* (
           (yomi (buffer-substring-no-properties b e))
	   (surrounding-text (sumibi-extract-lines-around-point b (ceiling (/ (max 2 sumibi-surrounding-lines) 2))))
           (henkan-list (sumibi-henkan-request yomi surrounding-text inverse-flag nil)))
      (setq sumibi-genbun yomi)
      (if henkan-list
          (condition-case err
	      (progn
                (setq
                 ;; 変換結果の保持
                 sumibi-henkan-kouho-list henkan-list
                 ;; 文節選択初期化
                 sumibi-cand-cur 0
                 ;; 
                 sumibi-cand-len (length henkan-list))
                
                (sumibi-debug-print (format "sumibi-henkan-kouho-list:%s \n" sumibi-henkan-kouho-list))
                (sumibi-debug-print (format "sumibi-cand-cur:%s \n" sumibi-cand-cur))
                (sumibi-debug-print (format "sumibi-cand-len:%s \n" sumibi-cand-len))

                ;; 同期で変換が成功した場合は、変換候補の保存を行う
                (if (eq (char-before b) ?/)
                    (setq b (- b 1)))
                (setq sumibi-last-roman (buffer-substring-no-properties b e))
                (delete-region b e)
                (goto-char b)
                (insert (sumibi-get-display-string))
                (sumibi--ensure-space-after-heading b)
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


(defun sumibi-henkan-region-async (b e inverse-flag)
  "リージョンをローマ字漢字変換する(非同期関数バージョン).
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument INVERSE-FLAG：逆変換かどうか"
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
        (sumibi-henkan-request
         yomi
	 yomi
         inverse-flag
         (lambda ()
           (with-current-buffer cur-buf
             (save-excursion
	       (delete-overlay yomi-overlay)
	       (delete-region (marker-position saved-b-marker)
			      (marker-position saved-e-marker))))))))))

(defun sumibi-henkan-region (b e inverse-flag)
  "指定された region を漢字変換する.  同期か非同期かはBからEまでの文字数で決定する.
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument INVERSE-FLAG：逆変換かどうか"
  (sumibi-init)
  (when sumibi-init
    (when (/= b e)
      (if (eq sumibi-backend 'mozc)
          ;; Mozc backend: セグメントごとに分割して処理
          (sumibi-henkan-region-mozc-segments b e inverse-flag)
        ;; 他のbackend: 従来通り
        (if (sumibi-determine-sync-p (buffer-substring-no-properties b e))
            (sumibi-henkan-region-sync b e inverse-flag)
          (sumibi-henkan-region-async b e inverse-flag))))))

(defun sumibi-henkan-region-mozc-segments (b e inverse-flag)
  "Mozc backend用のセグメント分割変換処理.
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument INVERSE-FLAG：逆変換かどうか"
  (let* ((region-text (buffer-substring-no-properties b e))
         (segments (split-string region-text "[ \t]+" t)))
    (setq sumibi-genbun region-text)
    (if (> (length segments) 1)
        ;; 複数セグメントの場合：各セグメントを個別に変換
        (sumibi-henkan-region-mozc-multiple-segments b e segments inverse-flag)
      ;; 単一セグメントの場合：従来通り
      (if (sumibi-determine-sync-p region-text)
          (sumibi-henkan-region-sync b e inverse-flag)
        (sumibi-henkan-region-async b e inverse-flag)))))

(defun sumibi-henkan-region-mozc-multiple-segments (b e segments inverse-flag)
  "複数セグメントを個別に変換する.
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument SEGMENTS: セグメントのリスト
Argument INVERSE-FLAG：逆変換かどうか"
  (let ((current-pos b)
        (original-text (buffer-substring-no-properties b e)))
    (save-excursion
      (goto-char b)
      (delete-region b e)
      (dolist (segment segments)
        (when (> (length segment) 0)
          (let ((segment-start (point))
                (segment-end (+ (point) (length segment))))
            (insert segment)
            ;; 個別セグメントを変換
            (sumibi-henkan-region-sync segment-start segment-end inverse-flag)
            ;; セグメント間にスペースを挿入しない（元の仕様通り）
            ))))))


(defun sumibi-char-charset (ch)
  "引数CHで指定した、カーソル前の文字種を返却する."
  (let ((result (char-charset ch)))
    (if (multibyte-string-p (char-to-string ch))
        'japanese-jisx0208
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo 情報の制御
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo buffer 退避用変数
(defvar sumibi-buffer-undo-list nil)
(make-variable-buffer-local 'sumibi-buffer-undo-list)
(defvar sumibi-buffer-modified-p nil)
(make-variable-buffer-local 'sumibi-buffer-modified-p)

(defvar sumibi-blink-cursor nil)
(defvar sumibi-cursor-type nil)

(defun sumibi-disable-undo ()
  "Undo bufferを退避し、undo 情報の蓄積を停止する."
  (when (not (eq buffer-undo-list t))
    (setq sumibi-buffer-undo-list buffer-undo-list)
    (setq sumibi-buffer-modified-p (buffer-modified-p))
    (setq buffer-undo-list t)))

(defun sumibi-enable-undo ()
  "退避した undo buffer を復帰し、undo 情報の蓄積を再開する."
  (when (not sumibi-buffer-modified-p) (set-buffer-modified-p nil))
  (when sumibi-buffer-undo-list
    (setq buffer-undo-list sumibi-buffer-undo-list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 現在の変換エリアの表示を行う
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sumibi-get-display-string ()
  "変換結果文字列を返す."
  (let* ((kouho      (nth sumibi-cand-cur sumibi-henkan-kouho-list))
         (_          (sumibi-debug-print (format "sumibi-cand-cur=%s\n" sumibi-cand-cur)))
         (_          (sumibi-debug-print (format "kouho=%s\n" kouho)))
         (word       (car kouho))
         (annotation (cadr kouho)))
    (sumibi-debug-print (format "word:[%d] %s(%s)\n" sumibi-cand-cur word annotation))
    word))

(defun sumibi-display-function (b e select-mode)
  "変換結果文字列をバッファに埋め込む形で表示する.
Argument B: リージョンの開始位置
Argument E: リージョンの終了位置
Argument SELECT-MODE：選択状態"
  (let ((insert-word (sumibi-get-display-string))
        (word (buffer-substring-no-properties b e)))
    (cond
     ((and (not select-mode)
           (string-equal insert-word word))
      ;; sumibi-markersの更新
      (setq sumibi-fence-start (progn
                                 (goto-char b)
                                 (point-marker)))
      (setq sumibi-fence-end   (progn
                                 (goto-char e)
                                 (point-marker)))
      (setq sumibi-markers
            (cons sumibi-fence-start sumibi-fence-end))

      ;; 確定文字列の作成
      (setq sumibi-last-fix insert-word)
      
      (sumibi-debug-print (format "don't touch:[%s] point:%d-%d\n" insert-word (marker-position sumibi-fence-start) (marker-position sumibi-fence-end))))

     (t
      (setq sumibi-henkan-separeter (if sumibi-use-fence " " ""))
      (when sumibi-henkan-kouho-list
        ;; UNDO抑制開始
        (sumibi-disable-undo)
        
        (delete-region b e)

        ;; リスト初期化
        (setq sumibi-markers '())

        (setq sumibi-last-fix "")

        ;; 変換したpointの保持
        (setq sumibi-fence-start (point-marker))
        (when select-mode (insert "|"))
        
        (let* (
	       (start       (point-marker)))
          (progn
            (insert insert-word)
            (sumibi--ensure-space-after-heading (marker-position start))
            (let* ((end         (point-marker))
                   (ov          (make-overlay start end)))
	      
	      ;; 確定文字列の作成
	      (setq sumibi-last-fix insert-word)
	      
	      ;; 選択中の場所を装飾する。
	      (when select-mode
                (overlay-put ov 'face 'default)
                (overlay-put ov 'face 'highlight))
	      (setq sumibi-markers (cons start end))
	      (sumibi-debug-print (format "insert:[%s] point:%d-%d\n" insert-word (marker-position start) (marker-position end))))))
        
        ;; fenceの範囲を設定する
        (when select-mode (insert "|"))
        (setq sumibi-fence-end   (point-marker))
        
        (sumibi-debug-print (format "total-point:%d-%d\n"
                                    (marker-position sumibi-fence-start)
                                    (marker-position sumibi-fence-end)))
        ;; UNDO再開
        (sumibi-enable-undo))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 変換候補選択モード
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar sumibi-popup-menu-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map "\r"        'popup-select)
    (define-key map "\C-f"      'popup-open)
    (define-key map [right]     'popup-open)
    (define-key map "\C-b"      'popup-close)
    (define-key map [left]      'popup-close)

    (define-key map "\C-n"      'popup-next)
    (define-key map "\C-j"      'popup-next)
    (define-key map (kbd "SPC") 'popup-next)
    (define-key map [down]      'popup-next)
    (define-key map "\C-p"      'popup-previous)
    (define-key map [up]        'popup-previous)

    (define-key map [f1]        'popup-help)
    (define-key map (kbd "\C-?") 'popup-help)

    (define-key map "\C-s"      'popup-isearch)
    (define-key map "\C-g"      'popup-close)
    (define-key map "\C-r"      'popup-select)
    map))



(defun sumibi-select-operation-inc ()
  "選択操作回数のインクリメント."
  (cl-incf sumibi-select-operation-times)
  (when (< 3 sumibi-select-operation-times)
    (sumibi-select-operation-reset)
    (let* ((lst
            (mapcar
             (lambda (x)
	       (concat
                (nth sumibi-tango-index x)
                "   ; "
                (nth sumibi-annotation-index x)))
             sumibi-henkan-kouho-list))
           (result
            (popup-menu* lst
                         :scroll-bar t
                         :margin t
                         :keymap sumibi-popup-menu-keymap
                         :initial-index sumibi-cand-cur)))
      (let ((selected-word (car (split-string result " "))))
        (setq sumibi-cand-cur (sumibi-find-by-tango selected-word))))))



(defun sumibi-select-operation-reset ()
  "選択操作回数のリセット."
  (setq sumibi-select-operation-times 0))

(defun sumibi-kakutei-and-self-insert (_arg)
  "候補選択を確定し、入力された文字を入力する.
_ARG: (未使用)"
  (interactive "P")
  (sumibi-select-kakutei)
  (setq unread-command-events (list last-command-event)))

(defun sumibi-select-update-display ()
  "候補選択状態での表示更新."
  (sumibi-display-function
   (marker-position sumibi-fence-start)
   (marker-position sumibi-fence-end)
   sumibi-select-mode))

(defun sumibi-select-kakutei ()
  "候補選択を確定する."
  (interactive)
  ;; 候補番号リストをバックアップする。
  (sumibi-debug-print (format "sumibi-select-kakutei\n"))
  (setq sumibi-cand-cur-backup sumibi-cand-cur)
  
  ;; genbunが設定されていない場合、候補リストから"原文まま"の候補を取得
  (when (or (not sumibi-genbun) (string= sumibi-genbun ""))
    (catch 'found
      (dolist (candidate sumibi-henkan-kouho-list)
        (when (and (listp candidate) 
                   (>= (length candidate) 2)
                   (string-equal (nth 1 candidate) "原文まま"))
          (setq sumibi-genbun (nth 0 candidate))
          (sumibi-debug-print (format "sumibi-select-kakutei: genbun set from 原文まま candidate: %S\n" sumibi-genbun))
          (throw 'found t)))))
  
  (setq sumibi-select-mode nil)
  (run-hooks 'sumibi-select-mode-end-hook)
  (sumibi-select-operation-reset)
  (sumibi-select-update-display)
  (sumibi-history-push))

(defun sumibi-select-cancel ()
  "候補選択をキャンセルする."
  (interactive)
  ;; カレント候補番号をバックアップしていた候補番号で復元する。
  (setq sumibi-cand-cur sumibi-cand-cur-backup)
  (setq sumibi-select-mode nil)
  (run-hooks 'sumibi-select-mode-end-hook)
  (sumibi-select-update-display)
  (sumibi-history-push))

(defun sumibi-select-prev ()
  "前の候補に進める."
  (interactive)
  ;; 前の候補に切りかえる
  (cl-decf sumibi-cand-cur)
  (when (> 0 sumibi-cand-cur)
    (setq sumibi-cand-cur (- sumibi-cand-len 1)))
  (sumibi-select-operation-inc)
  (sumibi-select-update-display))

(defun sumibi-select-next ()
  "次の候補に進める."
  (interactive)
  ;; 次の候補に切りかえる
  (setq sumibi-cand-cur
        (if (< sumibi-cand-cur (- sumibi-cand-len 1))
            (+ sumibi-cand-cur 1)
          0))
  (sumibi-select-operation-inc)
  (sumibi-select-update-display))

(defun sumibi-find-by-tango ( tango )
  "指定された TANGO のindex番号を返す."
  (let ((result-index nil))
    (mapc
     (lambda (x)
       (when (string-equal (nth sumibi-tango-index x) tango)
         (setq result-index (nth sumibi-id-index x))))
     sumibi-henkan-kouho-list)
    (sumibi-debug-print (format "sumibi-find-by-tango: tango=%s result=%S \n" tango result-index))
    result-index))

(defun sumibi-select-by-type-filter (type)
  "指定された TYPE の候補を抜き出す."
  (let ((lst '()))
    (mapc
     (lambda (x)
       (let ((sym (nth sumibi-kind-index x)))
         (when (eq sym type)
           (push x lst))))
     sumibi-henkan-kouho-list)
    (sumibi-debug-print (format "filtered-lst = %S\n" (reverse lst)))
    (if (null lst)
        nil
      (reverse lst))))


(defun sumibi-include-typep (type)
  "指定された TYPE の候補が存在するか調べる."
  (sumibi-select-by-type-filter type))

(defun sumibi-select-by-type (type)
  "指定された TYPE の候補に強制的に切りかえる.
切りかえが成功したかどうかを t or nil で返す"
  (let ((kouho (car (sumibi-select-by-type-filter type))))
    (if (not kouho)
        (progn
          (cond
           ((eq type 'j)
            (message "Sumibi: 漢字の候補はありません。"))
           ((eq type 'h)
            (message "Sumibi: ひらがなの候補はありません。"))
           ((eq type 'k)
            (message "Sumibi: カタカナの候補はありません。"))
           ((eq type 'l)
            (message "Sumibi: 半角の候補はありません。"))
           ((eq type 'z)
            (message "Sumibi: 全角の候補はありません。"))
           ((eq type 'n)
            (message "Sumibi: 数字混在の候補はありません．")))
          nil)
      (let ((num   (nth sumibi-id-index kouho)))
        (setq sumibi-cand-cur num)
        (sumibi-select-update-display)
        t))))

(defun sumibi-select-kanji ()
  "漢字候補に強制的に切りかえる."
  (interactive)
  (sumibi-select-by-type 'j))

(defun sumibi-select-hiragana ()
  "ひらがな候補に強制的に切りかえる."
  (interactive)
  (sumibi-select-by-type 'h))

(defun sumibi-select-katakana ()
  "カタカナ候補に強制的に切りかえる."
  (interactive)
  (sumibi-select-by-type 'k))

(defun sumibi-select-hankaku ()
  "半角候補に強制的に切りかえる."
  (interactive)
  (sumibi-select-by-type 'l))

(defun sumibi-select-zenkaku ()
  "半角候補に強制的に切りかえる."
  (interactive)
  (sumibi-select-by-type 'z))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 変換履歴操作関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun sumibi-history-gc ()
  "変数sumibi-history-stack中の無効なマークを持つエントリを削除する."
  (let ((temp-list '()))
    (mapc
     (lambda (alist)
       (let ((markers  (sumibi-assoc-ref 'markers  alist nil)))
	 ;; markersがnilまたは無効な場合の安全チェック
	 (if (and markers 
                  (consp markers) 
                  (markerp (car markers)) 
                  (markerp (cdr markers)))
             (progn
               (if (and (marker-position (car markers)) ;; 存在するバッファを指しているか
			(marker-position (cdr markers)))
                   (if (= (marker-position (car markers))
                          (marker-position (cdr markers)))
	               ;; マークの開始と終了が同じ位置を指している場合は、
	               ;; そのマークは既に無効(選択モードの再表示で一旦マーク周辺の文字列が削除された)
	               (progn
			 (set-marker (car markers) nil)
			 (set-marker (cdr markers) nil))
                     (push alist temp-list))))
           ;; markersがnilまたは無効な場合のハンドリング
           (progn
             ;; markersがnilの場合でも、他のデータが有効であれば履歴として保持
             (push alist temp-list)))))
     sumibi-history-stack)

    ;; temp-list から limit 件数だけコピーする
    (setq sumibi-history-stack '())
    (mapc
     (lambda (alist)
       (when (< (length sumibi-history-stack)
		sumibi-history-stack-limit)
	 (push alist sumibi-history-stack)))
     (reverse temp-list))))

(defun sumibi-history-search (_point load-flag)
  "確定ヒストリから、指定_POINTに変換済の単語が埋まっているかどうか調べる.
戻り値: t か nil を返す
補足: LOAD-FLAG に 真を渡すと、見付かった情報で、現在の変換候補変数にロードしてくれる"
  (sumibi-history-gc)

  ;; カーソル位置に有効な変換済エントリがあるか探す
  (let ((found nil))
    (mapc
     (lambda (alist)
       (let* ((markers  (sumibi-assoc-ref 'markers  alist nil))
	      (last-fix (sumibi-assoc-ref 'last-fix alist ""))
	      (bufname  (sumibi-assoc-ref 'bufname alist "")))
	 ;; markersがnilまたは無効な場合はスキップ
	 (when (and markers 
                    (consp markers) 
                    (markerp (car markers)) 
                    (markerp (cdr markers))
                    (marker-position (cdr markers)))
           (let* ((end      (marker-position (cdr markers)))
	          (start    (- end (length last-fix)))
	          (pickup   (if (string-equal bufname (buffer-name))
				(buffer-substring start end)
			      "")))
             (when (and
                    (string-equal bufname (buffer-name))
                    (<  start   (point))
                    (<= (point) end)
                    (string-equal last-fix pickup))
	       (setq found t)
	       (when load-flag
		 ;; markersがnilでない場合のみmarkersを更新
		 (when (and (not (null markers))
                            (markerp (car markers))
                            (markerp (cdr markers)))
                   (setq sumibi-markers            (cons
					            (move-marker (car markers) start)
					            (cdr markers))))
		 ;; その他の変数は常に更新
		 (setq sumibi-cand-cur           (sumibi-assoc-ref 'cand-cur alist           nil))
		 (setq sumibi-cand-cur-backup    (sumibi-assoc-ref 'cand-cur-backup alist    nil))
		 (setq sumibi-cand-len           (sumibi-assoc-ref 'cand-len alist           nil))
		 (setq sumibi-last-fix           pickup)
		 (setq sumibi-genbun             (sumibi-assoc-ref 'genbun alist             nil))
		 (setq sumibi-henkan-kouho-list  (sumibi-assoc-ref 'henkan-kouho-list alist  nil))

		 ))))))
     sumibi-history-stack)
    found))

(defun sumibi-string-ascii-only-p (str)
  "STRがアルファベット・数字・記号のみで構成されているかチェックする."
  (when (stringp str)
    ;; テキストプロパティを除去してからチェック
    (let ((plain-str (substring-no-properties str)))
      (string-match-p "\\`[[:ascii:]]+\\'" plain-str))))

(defun sumibi-history-push ()
  "確定ヒストリにエントリーを追加する.
アルファベットのみの確定結果は履歴に追加しない."
  ;; アルファベットのみの確定結果はスキップ
  (if (sumibi-string-ascii-only-p sumibi-last-fix)
      (sumibi-debug-print (format "sumibi-history-push: skipping ASCII-only result: %S\n" sumibi-last-fix))
    (progn
      (push
       `(
         (markers            . ,sumibi-markers            )
         (cand-cur           . ,sumibi-cand-cur           )
         (cand-cur-backup    . ,sumibi-cand-cur-backup    )
         (cand-len           . ,sumibi-cand-len           )
         (last-fix           . ,sumibi-last-fix           )
         (last-roman         . ,sumibi-last-roman         )
         (genbun             . ,sumibi-genbun             )
         (henkan-kouho-list  . ,sumibi-henkan-kouho-list  )
         (bufname            . ,(buffer-name)))
       sumibi-history-stack)
      ;; 履歴スタックのサイズを制限
      (when (> (length sumibi-history-stack) sumibi-history-stack-limit)
        (setq sumibi-history-stack
              (seq-take sumibi-history-stack sumibi-history-stack-limit))
        (sumibi-debug-print (format "sumibi-history-push: trimmed stack to %d entries\n"
                                    sumibi-history-stack-limit)))))
  ;; --------------------------------------------------------------
  ;; Mozc learning (optional)
  ;; --------------------------------------------------------------
  (sumibi-debug-print (format "sumibi-history-push: (sumibi-backend-mozc-p)=%s sumibi--mozc-available-p=%s sumibi-last-roman=%s sumibi-last-fix=%s\n" (sumibi-backend-mozc-p) sumibi--mozc-available-p sumibi-last-roman sumibi-last-fix))
  (when (and sumibi-mozc-learn-at-kakutei
             (sumibi-backend-mozc-p)
             sumibi--mozc-available-p
             (stringp sumibi-last-roman)
             (> (length sumibi-last-roman) 0)
             (stringp sumibi-last-fix)
             (> (length sumibi-last-fix) 0))
    (sumibi--mozc-learn sumibi-last-roman sumibi-last-fix))
  (sumibi-debug-save-dashboard))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ローマ字漢字変換関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sumibi-rK-trans ()
  "ローマ字漢字変換をする.
カーソルから行頭方向にローマ字列が続く範囲でローマ字漢字変換を行う."
  (interactive)
  (sumibi-debug-print "sumibi-rK-trans()")

  (cond
   ;; region指定している場合
   ((region-active-p)
    (let ((b (region-beginning))
          (e (region-end)))
      (sumibi-henkan-region b e nil)))

   ;; region指定していない場合
   (t
    ;; 最後に変換した行番号の更新
    (setq sumibi-last-lineno (line-number-at-pos (point)))

    (cond
     (sumibi-select-mode
      (sumibi-debug-print "<<sumibi-select-mode>>\n")
      ;; 候補選択モード中に呼出されたら、keymapから再度候補選択モードに入る
      (funcall (lookup-key sumibi-select-mode-map sumibi-rK-trans-key)))

     (t
      (cond

       ((eq (sumibi-char-charset (preceding-char)) 'ascii)
        (sumibi-debug-print (format "ascii? (%s) => t\n" (preceding-char)))
        ;; カーソル直前が alphabet だったら
        (let ((end (point))
	      (gap (sumibi-skip-chars-backward)))
          (when (/= gap 0)
            ;; 意味のある入力が見つかったので変換する
            (let (
                  (b (+ end gap))
                  (e end))
	      (sumibi-henkan-region b e nil)))))
       
       ((or (sumibi-kanji (preceding-char))
            (sumibi-nkanji (preceding-char)))
        (sumibi-debug-print (format "sumibi-kanji(%s) or sumibi-nkanji(%s) => t\n"
                                    (preceding-char)
                                    (preceding-char)))
        ;; カーソル直前が 全角で漢字以外 だったら候補選択モードに移行する。
        ;; また、最後に確定した文字列と同じかどうかも確認する。
        (when (sumibi-history-search (point) t)
          ;; 直前に変換したfenceの範囲に入っていたら、候補選択モードに移行する。
          ;; ただし、markersが有効な場合のみ
          (when (and sumibi-markers
                     (markerp (car sumibi-markers))
                     (markerp (cdr sumibi-markers)))
            (setq sumibi-select-mode t)
            (sumibi-debug-print "henkan mode ON\n")
            
            ;; 表示状態を候補選択モードに切替える。
            (sumibi-display-function
             (marker-position (car sumibi-markers))
             (marker-position (cdr sumibi-markers))
             t))))

       (t
        (sumibi-debug-print (format "<<OTHER:non-ascii,non-kanji>> (%s)\n" (preceding-char))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 英語への翻訳関数
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sumibi-english-trans ()
  "英語への翻訳を行う."
  (interactive)
  (sumibi-debug-print "sumibi-english-trans()")

  ;; region指定している場合しか発動しない。
  (when (region-active-p)
    (let ((b (region-beginning))
          (e (region-end)))
      (sumibi-henkan-region b e t))))


(defun sumibi-string-include-kanji (str)
  "STRが漢字を含む文字列であるかどうかの判定関数."
  (let ((kanji-lst
         (-filter
          (lambda (x)
            (if (string-equal x "")
                nil
	      (sumibi-kanji (string-to-char x))))
          (split-string str ""))))
    (< 0 (length kanji-lst))))

(defun sumibi-nkanji (ch)
  "文字CHが全角で漢字以外かを判定する."
  (and (eq (sumibi-char-charset ch) 'japanese-jisx0208)
       (not (string-match "[亜-黑]" (char-to-string ch)))))


(defun sumibi-kanji (ch)
  "文字CHが全角で漢字かを判定する."
  (and (eq (sumibi-char-charset ch) 'japanese-jisx0208)
       (string-match "[亜-黑]" (char-to-string ch))))


(defun sumibi-skip-chars-backward ()
  "ローマ字漢字変換時、変換対象とするローマ字を読み飛ばす."
  (let* (
         (skip-chars
          (if auto-fill-function
	      ;; auto-fill-mode が有効になっている場合改行があってもskipを続ける
	      (concat sumibi-skip-chars "\n")
            ;; auto-fill-modeが無効の場合はそのまま
            sumibi-skip-chars))
         
         ;; マークされている位置を求める。
         (pos (or (and (markerp (mark-marker)) (marker-position (mark-marker)))
                  1))

         ;; 条件にマッチする間、前方方向にスキップする。
         (result (save-excursion
                   (skip-chars-backward skip-chars (and (< pos (point)) pos))))
         (limit-point 0))

    (if auto-fill-function
        ;; auto-fill-modeが有効の時
        (progn
          (save-excursion
            (backward-paragraph)
            (when (< 1 (point))
	      (forward-line 1))
            (beginning-of-line)
            (let (
                  (start-point (point)))
	      (setq limit-point
                    (+
                     start-point
                     (skip-chars-forward sumibi-stop-chars (point-at-eol))))))

          ;; (sumibi-debug-print (format "(point) = %d  result = %d  limit-point = %d\n" (point) result limit-point))
          ;; (sumibi-debug-print (format "a = %d b = %d \n" (+ (point) result) limit-point))

          ;; パラグラフ位置でストップする
          (if (< (+ (point) result) limit-point)
	      (-
	       limit-point
	       (point))
            result))

      ;; auto-fill-modeが無効の時
      (progn
        (save-excursion
          (beginning-of-line)
          (let (
                (start-point (point)))
            (setq limit-point
                  (+
                   start-point
                   (skip-chars-forward sumibi-stop-chars (point-at-eol))))))

        ;; ------------------------------------------------------
        ;; 行頭に変換対象外の先頭記号 (- , *) が来ていたら 2 文字分スキップ
        ;; (auto-fill-mode 無効時のみ)
        ;; ------------------------------------------------------
        (let ((bol (save-excursion (beginning-of-line) (point))))
          (when (= limit-point bol)
            (let ((prefix (and (>= (point-at-eol) (+ bol 2))
			       (buffer-substring-no-properties bol (+ bol 2)))))
	      (when (member prefix '("- " "* "))
                (setq limit-point (+ bol 2)))))

          ;; 行頭に連続する空白/タブがインデントとして存在するときもスキップ
          (when (= limit-point bol)
            (let ((indent-len (save-excursion
                                (goto-char bol)
                                (skip-chars-forward " \t" (point-at-eol)))))
	      (sumibi-debug-print (format "indent-len = %d\n" indent-len))
	      (when (> indent-len 0)
                (setq limit-point (+ bol indent-len))
                ;; --------------------------------------------------
                ;; When the line begins with indentation followed by a
                ;; list marker ("- " or "* "), also skip those two
                ;; characters so that the conversion starts *after*
                ;; the marker.  This makes constructs such as
                ;; "  - koumoku" correctly convert only the roman
                ;; letters part ("koumoku"), keeping the list prefix
                ;; intact.  Existing behaviour for non-indented list
                ;; items is preserved by the earlier branch that runs
                ;; when `limit-point` equals `bol`.
                (let* ((prefix-pos limit-point)
		       (eol (point-at-eol))
		       (prefix (and (>= eol (+ prefix-pos 2))
                                    (buffer-substring-no-properties prefix-pos (+ prefix-pos 2)))))
                  (when (member prefix '("- " "* "))
                    (setq limit-point (+ prefix-pos 2))))))))

        ;; (sumibi-debug-print (format "(point) = %d  result = %d  limit-point = %d\n" (point) result limit-point))
        ;; (sumibi-debug-print (format "a = %d b = %d \n" (+ (point) result) limit-point))

        (if (< (+ (point) result) limit-point)
            ;; インデント位置でストップする。
            (-
             limit-point
             (point))
          result)))))


(defun sumibi-insert-space (times)
  "TIMESで指定した回数スペース文字を挿入する."
  (if (null times)
      (insert " ")
    (dotimes(_ times)
      (insert " "))))

(defun sumibi-switch-model (&optional _arg)
  "GPTのモデルを切り替える.
引数_ARG: 未使用"
  (interactive "P")
  (if (getenv "SUMIBI_AI_MODEL")
      (message "!! 環境変数SUMIBI_AI_MODELが設定されているときは、モデルを動的にスイッチできません !!")
    (let ((index 
	   (cl-position-if
	    (lambda (item)
	      (and (stringp item)
		   (string-match-p sumibi-current-model item)))
	    sumibi-model-list)))
      (let ((result
	     (popup-menu* sumibi-model-list
			  :scroll-bar t
			  :margin t
			  :keymap sumibi-popup-menu-keymap
			  :initial-index index)))
	(setq sumibi-current-model result)))))

;; sumibi-mode の状態変更関数
;;  正の引数の場合、常に sumibi-mode を開始する
;;  {負,0}の引数の場合、常に sumibi-mode を終了する
;;  引数無しの場合、sumibi-mode をトグルする

;; buffer 毎に sumibi-mode を変更する
(defun sumibi-mode (&optional arg)
  "Sumibi mode は ローマ字から直接漢字変換するための minor mode です.
引数に正数を指定した場合は、Sumibi mode を有効にします

Sumibi モードが有効になっている場合 \\<sumibi-mode-map>\\[sumibi-rK-trans] で
point から行頭方向に同種の文字列が続く間を漢字変換します

同種の文字列とは以下のものを指します
・半角カタカナとsumibi-stop-chars に指定した文字を除く半角文字
・漢字を除く全角文字
引数 ARG は未使用"
  (interactive "P")
  (sumibi-mode-internal arg nil))

;; 全バッファで sumibi-mode を変更する
(defun global-sumibi-mode (&optional arg)
  "Sumibi mode は ローマ字から直接漢字変換するための minor mode です.
引数に正数を指定した場合は、Sumibi mode を有効にします

Sumibi モードが有効になっている場合 \\<sumibi-mode-map>\\[sumibi-rK-trans] で
point から行頭方向に同種の文字列が続く間を漢字変換します

同種の文字列とは以下のものを指します。
・半角カタカナとsumibi-stop-chars に指定した文字を除く半角文字
・漢字を除く全角文字
引数 ARG は未使用"
  (interactive "P")
  (sumibi-mode-internal arg t))



(defun sumibi-mode-internal (arg global)
  "Sumibi modeを変更する共通関数.
引数ARG: 未使用
引数GLOBAL: Sumibi modeをどのバッファでも有効にするグローバルモードにする"
  (sumibi-debug-print "sumibi-mode-internal :1\n")

  (or (local-variable-p 'sumibi-mode (current-buffer))
      (make-local-variable 'sumibi-mode))
  (if global
      (progn
        (setq default-input-method "japanese-sumibi")
        (setq-default sumibi-mode (if (null arg) (not sumibi-mode)
                                    (> (prefix-numeric-value arg) 0)))
        (sumibi-kill-sumibi-mode))
    (setq sumibi-mode (if (null arg) (not sumibi-mode)
                        (> (prefix-numeric-value arg) 0))))
  (when sumibi-mode (run-hooks 'sumibi-mode-hook))

  (sumibi-debug-print "sumibi-mode-internal :2\n"))



(defun sumibi-kill-sumibi-mode ()
  "バッファローカルな `sumibi-mode` を削除する."
  (let ((buf (buffer-list)))
    (while buf
      (set-buffer (car buf))
      (kill-local-variable 'sumibi-mode)
      (setq buf (cdr buf)))))


(defun sumibi-mode-line-function ()
  "この関数はモードラインをクリックで呼び出され、モデルをスイッチできます."
  (interactive)
  (sumibi-switch-model))

(defvar sumibi-mode-line-string
  (propertize " [sumibi-switch-model] "
	      'help-echo "クリックして利用するGPTのモデルを切り替えることができます."
	      'mouse-face 'mode-line-highlight
	      'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line mouse-1] 'sumibi-mode-line-function)
                           map)))

;; sumibi-mode有効時にモードラインにモデルスイッチの機能を追加する.
(add-hook 'sumibi-mode-hook
	  (lambda ()
	    (setq-default mode-line-format (delq sumibi-mode-line-string mode-line-format))
	    (setq-default mode-line-format (append mode-line-format (list sumibi-mode-line-string)))))

;; 全バッファで sumibi-input-mode を変更する
(defun sumibi-input-mode (&optional arg)
  "入力モード変更.
引数ARG: 未使用"
  (interactive "P")
  (if (< 0 arg)
      (progn
        (setq deactivate-current-input-method-function 'sumibi-inactivate)
        (setq sumibi-mode t))
    (progn
      (setq deactivate-current-input-method-function nil)
      (setq sumibi-mode nil))))

;; input method 対応
(defun sumibi-activate (&rest _arg)
  "入力モードを有効にする.
引数_ARG: 未使用"
  (sumibi-input-mode 1))
(defun sumibi-inactivate (&rest _arg)
  "入力モードを無効にする.
引数_ARG: 未使用"
  (sumibi-input-mode -1))
(register-input-method
 "japanese-sumibi" "Japanese" 'sumibi-activate
 "" "Roman -> Kanji&Kana"
 nil)

;; input-method として登録する。
(set-language-info "Japanese" 'input-method "japanese-sumibi")


;; 履歴保存機能 - ディレクトリとファイルの作成
(defun sumibi-ensure-history-directory ()
  "~/.sumibi ディレクトリを作成する（存在しない場合）."
  (let ((dir (file-name-directory (expand-file-name sumibi-history-file-path))))
    (unless (file-directory-p dir)
      (make-directory dir t))))

(defun sumibi-load-history-from-file ()
  "履歴ファイルから履歴を読み込む."
  (let ((file-path (expand-file-name sumibi-history-file-path)))
    (when (file-exists-p file-path)
      ;; 履歴を読み込む前に既存のスタックをクリア
      (setq sumibi-history-stack '())
      (let ((success-count 0)
            (error-count 0))
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties 
                         (line-beginning-position) (line-end-position))))
              (when (and (stringp line) (> (length line) 0))
                (condition-case err
                    (let ((entry (json-read-from-string line)))
                      ;; JSON形式のalistをEmacs Lisp形式に変換
                      (let ((converted-entry '()))
                        (dolist (pair entry)
                          (let ((key (car pair))
                                (value (cdr pair)))
                            ;; markersフィールドの特別処理
                            (if (eq key 'markers)
                                (if (vectorp value)
                                    ;; 配列の場合はnilに変換（後でmarkerを作成することはできない）
                                    (push (cons key nil) converted-entry)
                                  ;; それ以外はそのまま
                                  (push (cons key value) converted-entry))
                              ;; その他のフィールドはそのまま
                              (push (cons key value) converted-entry))))
                        ;; 履歴スタックに追加（新しいものを先頭に）
                        (setq sumibi-history-stack (append sumibi-history-stack (list converted-entry)))
                        (setq success-count (1+ success-count))))
                  (error
                   (setq error-count (1+ error-count)))))
              (forward-line 1))))
        ;; 履歴スタックのサイズを制限
        (when (> (length sumibi-history-stack) sumibi-history-stack-limit)
          (let ((old-length (length sumibi-history-stack)))
            ;; 最新のsumibi-history-stack-limit件のみを保持（リストの末尾から取る）
            (setq sumibi-history-stack 
                  (nthcdr (- (length sumibi-history-stack) sumibi-history-stack-limit)
                          sumibi-history-stack))
            (sumibi-debug-print (format "Trimmed history stack from %d to %d entries\n"
                                        old-length (length sumibi-history-stack)))))
        (sumibi-debug-print (format "Loaded history from %s: %d success, %d error, %d in stack\n" 
                                    file-path success-count error-count
                                    (length sumibi-history-stack)))
        (list success-count error-count)))))

(defun sumibi-save-history-to-file ()
  "履歴をファイルに保存する."
  (sumibi-ensure-history-directory)
  (when sumibi-history-stack
    ;; 保存前にsumibi-history-stackを制限
    (when (> (length sumibi-history-stack) sumibi-history-stack-limit)
      (setq sumibi-history-stack
            (seq-take sumibi-history-stack sumibi-history-stack-limit))
      (sumibi-debug-print (format "sumibi-save-history-to-file: trimmed stack to %d entries\n"
                                  sumibi-history-stack-limit)))
    (let ((file-path (expand-file-name sumibi-history-file-path)))
      (with-temp-buffer
        (dolist (entry sumibi-history-stack)
          (let ((json-entry (copy-alist entry)))
            ;; markerオブジェクトまたはconsペアを配列に変換
            (when (assoc 'markers json-entry)
              (let ((markers (cdr (assoc 'markers json-entry))))
                (when (and markers (consp markers))
                  (if (and (markerp (car markers)) (markerp (cdr markers)))
                      ;; markerオブジェクトの場合は位置を取得
                      (setcdr (assoc 'markers json-entry) 
                              (vector (marker-position (car markers))
                                      (marker-position (cdr markers))))
                    ;; 既に数値のconsペアの場合はそのまま配列に変換
                    (setcdr (assoc 'markers json-entry) 
                            (vector (car markers) (cdr markers)))))))
            (insert (json-encode json-entry) "\n")))
        (write-region (point-min) (point-max) file-path nil 'silent))
      (sumibi-debug-print (format "Saved %d history entries to %s\n" 
                                  (length sumibi-history-stack) file-path)))))

(defconst sumibi-version
  "3.7.0" ;;SUMIBI-VERSION
  )
(defun sumibi-version (&optional _arg)
  "Sumibiのバージョン番号をミニバッファに表示する.
引数_ARG: 未使用"
  (interactive "P")
  (message sumibi-version))

;; Emacs終了時のフック設定
(add-hook 'kill-emacs-hook 'sumibi-save-history-to-file)

(provide 'sumibi)

;; Local Variables:
;; coding: utf-8
;; End:

;;; sumibi.el ends here
