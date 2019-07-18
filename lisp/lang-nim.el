;;; lang-nim.el --- Nim language support -*- lexical-binding: t -*-

;;; Commentary:
;; Nimble and quick.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package nim-mode
  :preface
  (defun aggressive-indent-mode-off ()
    (aggressive-indent-mode -1))
  :hook
  (nim-mode . nimsuggest-mode)
  (nim-mode . aggressive-indent-mode-off)
  (nimsuggest-mode . company-mode)
  (nimsuggest-mode . flymake-mode)
  :init
  (maybe-push-exec-path (expand-file-name "nimble/bin" xdg-data-home)))

(provide 'lang-nim)
;;; lang-nim.el ends here
