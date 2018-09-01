;;; lang-c.el --- C support -*- lexical-binding: t -*-

;;; Commentary:
;; Damn.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Functions

(defun +c-astyle-gnu-region (pmin pmax)
  "Formats the selected region using the GNU C formatting style."
  (interactive "r")
  (shell-command-on-region pmin pmax
                           "astyle -A7 -s2 -xB -xD"
                           (current-buffer) t
                           (get-buffer-create "*Astyle Errors*")
                           t))

(defun +c-astyle-gnu-current-buffer ()
  (interactive)
  (mark-whole-buffer)
  (call-interactively #'+c-astyle-gnu-region))

;;;
;; Packages

(req-package cc-mode
  :defer 1
  :general
  (:keymaps 'c-mode-map
            :states '(normal operator)
            :prefix xeal-localleader-key
            "=" #'+c-astyle-gnu-current-buffer)
  (:keymaps 'c-mode-map
            :states '(visual)
            :prefix xeal-localleader-key
            "=" #'+c-astyle-gnu-region))

(provide 'lang-c)
;;; lang-c.el ends here
