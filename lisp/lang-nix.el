;;; lang-nix.el --- Nix language support -*- lexical-binding: t -*-

;;; Commentary:
;; Functionally effective package management.

;;; Code:

(use-package nix-mode
  :mode "\\.nix$"
  :general
  (:keymaps 'nix-mode-map
   :states '(normal visual)
   :leader xeal-localleader-key
   "'" #'nix-repl))

(use-package company-nixos-options
  :hook
  (nix-mode . company-mode)
  :init
  (set-company-backends 'nix-mode 'company-nixos-options))

(provide 'lang-nix)
;;; lang-nix.el ends here
