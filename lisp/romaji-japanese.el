;;; romaji-japanese.el --- Interface for romaji to Japanese conversion -*- lexical-binding: t; -*-
;;; Commentary:
;; This file provides a simple interface to convert romaji to Japanese
;; by delegating to Sumibi's conversion functions.
;;; Code:

(defgroup romaji-japanese nil
  "Interface for romaji to Japanese conversion."
  :group 'input-method)

(defcustom romaji-japanese-convert-key sumibi-rK-trans-key
  "Key to trigger romaji to Japanese conversion."
  :type 'string
  :group 'romaji-japanese)

(defun romaji-japanese-trans ()
  "Convert preceding romaji text to Japanese using Sumibi."
  (interactive)
  (sumibi-rK-trans))

(defalias 'romaji-japanese-convert 'romaji-japanese-trans
  "Alias for `romaji-japanese-trans`.")

(provide 'romaji-japanese)
;;; romaji-japanese.el ends here
