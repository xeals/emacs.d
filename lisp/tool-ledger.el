;;; tool-ledger.el --- Ledger CLI support -*- lexical-binding: t -*-

;;; Commentary:
;; Money.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package ledger-mode
  :mode "\\.ledger$"
  :general
  (:keymaps 'ledger-mode-map
            :states insert
            "<C-tab>" '(λ! (insert "\\t"))))

(provide 'tool-ledger)
;;; tool-ledger.el ends here
