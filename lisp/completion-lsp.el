;;; completion-lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;;; Commentary:
;; Speaking everything at once.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package eglot
  ;; :defer 2 ;; FIXME ?
  :commands (eglot-ensure)
  :general
  (general-leader
    "cC" '(eglot :wk "connect to lsp"))
  (general-leader :keymaps 'eglot-mode-map
    "ce" '(:ignore t :wk "eglot")
    "ce=" #'eglot-format-buffer
    "cef" #'eglot-format
    "ceh" #'eglot-help-at-point
    "cem" #'eglot-events-buffer
    "cer" #'eglot-rename
    "ceR" #'eglot-reconnect)
  :config
  (add-to-list 'eglot-server-programs '(crystal-mode . ("scry"))))

(provide 'completion-lsp)
;;; completion-lsp.el ends here
