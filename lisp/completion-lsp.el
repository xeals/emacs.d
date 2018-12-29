;;; completion-lsp.el --- Language Server Protocol -*- lexical-binding: t -*-

;;; Commentary:
;; Speaking everything at once.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package eglot
  :el-get t :ensure nil
  :hook
  (rust-mode . eglot-ensure)
  :general
  (:states '(normal visual operator)
            :prefix xeal-leader-key
            "x" '(:ignore t :wk "eglot")
            "cC" #'eglot)
  (:keymaps 'eglot-mode-map
            :states '(normal visual operator)
            :prefix xeal-leader-key
            "ch" #'eglot-help-at-point
            "cM" #'eglot-events-buffer
            "cr" #'eglot-rename
            "cR" #'eglot-reconnect)
  :init
  (add-to-list 'eglot-server-programs '(crystal-mode . ("scry")))
  (dolist (mode (list 'rust-mode))
    (set-doc-fn mode #'eglot-help-at-point)))

(provide 'completion-lsp)
;;; completion-lsp.el ends here
