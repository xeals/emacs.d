;;; tool-term.el --- Terminal configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This is sorta nice once you get used to it.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-feature term
  :general
  (:keymaps 'term-mode-map
   :states 'emacs
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "C-l" #'evil-window-right)
  :init
  (after! evil
    ;; Default to emacs mode in shells
    (dolist (mode (list 'eshell-mode 'term-mode))
      (setq evil-insert-state-modes (delete mode evil-insert-state-modes))
      (push mode evil-emacs-state-modes))))

(provide 'tool-term)
;;; tool-term.el ends here
