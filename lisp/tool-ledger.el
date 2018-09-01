;;; tool-ledger.el --- Ledger accounting mode -*- lexical-binding: t -*-

;;; Commentary:
;; Finance is hard.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(req-package ledger-mode
  :demand t
  :mode "\\.ledger$"
  :general
  (:keymaps 'ledger-mode-map
            :states '(insert)
            "<C-tab>" '(λ! (insert "\\t"))))

(if (version< emacs-version "26.0")
  (req-package flycheck-ledger
    :hook (ledger-mode . flycheck-mode))
  (add-hook 'ledger-mode-hook 'flymake-mode))

(provide 'tool-ledger)
;;; tool-ledger.el ends here
