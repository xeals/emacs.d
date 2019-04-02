;;; tool-dired.el --- File browser -*- lexical-binding: t -*-

;;; Commentary:
;; When `counsel-find-file' doesn't suffice.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Packages

(use-feature dired
  :hook
  (dired-mode . hl-line-mode)
  :general
  (:keymaps 'dired-mode-map
            :major-modes t
            "SPC" #'nil
            "DEL" #'dired-up-directory
            "r"   #'dired-do-redisplay))

(use-package all-the-icons-dired
  :disabled t
  :commands all-the-icons-dired-mode
  :init
  (add-graphic-hook
   (add-hooks-pair 'dired-mode 'all-the-icons-dired-mode)))

(provide 'tool-dired)
;;; tool-dired.el ends here
