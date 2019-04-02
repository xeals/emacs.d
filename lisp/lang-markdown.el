;;; lang-markdown.el --- Markdown language support -*- lexical-binding: t -*-

;;; Commentary:
;; Pretty plaintext.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Packages

(use-package markdown-mode
  :mode (("README\\.md$" . gfm-mode)
         ("\\.md$"       . markdown-mode)
         ("\\.markdown$" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(provide 'lang-markdown)
;;; lang-markdown.el ends here
