;;; tool-direnv.el --- Direnv support -*- lexical-binding: t -*-

;;; Commentary:
;; Supposedly making dev life on Nix a bit easier.

;;; Code:

(req-package direnv
  :demand (nixos?)
  :config
  (direnv-mode)
  (advice-add #'direnv-edit :override
              (lambda () (message "`direnv-edit' is super broken. Don't do that. Open the .envrc manually."))))

(provide 'tool-direnv)
;;; tool-direnv.el ends here
