;;; -*- emacs-lisp -*-
;;; metropolis-theme.el --- A base16, GMK Metropolis-inspired colorscheme

;; Author: Alex Smith <xeals@pm.me>
;; Version: 0
;; Package-Requires: ((base16-theme "2.0"))
;; Keywords: theme

;; This file is not part of GNU Emacs.

;;; License:
;; Unlicense.

;;; Commentary:
;; Base16: (https://github.com/chriskempson/base16)

;;; Code:

(require 'base16-theme)

(defvar metropolis-colors
  '(:base00 "#0E1F2C"
    :base01 "#112636"
    :base02 "#12293A"
    :base03 "#373E45"
    :base04 "#4A4D4F"
    :base05 "#CFD2D4"
    :base06 "#E4E7EA"
    :base07 "#F7FAFD"
    :base08 "#D34728"
    :base09 "#E4913A"
    :base0A "#F4BC47"
    :base0B "#BBC832"
    :base0C "#55C3B7"
    :base0D "#6D99BD"
    :base0E "#9A77CF")
    ; :base0E "#D34728"
    ; :base0A "#E4913A"
    ; :base09 "#F4BC47"
    ; :base0B "#BBC832"
    ; :base0C "#55C3B7"
    ; :base0D "#6D99BD"
    ; :base08 "#9A77CF"
    ; :base0F "#A49688")
  "Colours for GMK Metropolis.")

(deftheme metropolis)

(base16-theme-define 'metropolis metropolis-colors)

(provide-theme 'metropolis)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide 'metropolis-theme)

;;; metropolis-theme.el ends here
