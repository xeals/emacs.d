;;; lang-ruby.el --- Ruby support -*- lexical-binding: t -*-

;;; Commentary:
;; Gems.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Packages

(req-package enh-ruby-mode
  :hook
  (ruby-mode . enh-ruby-mode)
  (enh-ruby-mode . rainbow-mode)
  :mode (("\\.rb$"      . enh-ruby-mode)
         ("\\.ru$"      . enh-ruby-mode)
         ("Gemfile"     . enh-ruby-mode)
         ("\\.gemfile$" . enh-ruby-mode)
         ("Rakefile$"   . enh-ruby-mode)
         ("\\.rake$"    . enh-ruby-mode)))

(req-package robe
  :hook
  (enh-ruby-mode . robe-mode)
  :general
  (:keymaps 'robe-mode-map
            :states '(normal visual operator)
            :prefix xeal-local-leader-key
            xeal-local-leader-key #'robe-start)
  :init
  ;; (defun +robe/start ()
  ;;   (interactive)
  ;;   (unless robe-running
  ;;     (robe-start)))
  ;; (defun +robe/auto-start ()
  ;;   (interactive)
  ;;   (unless robe-running
  ;;     (call-interactively #'inf-ruby)))

  ;; (defadvice inf-ruby-console-auto (after inf-ruby-console-auto activate)
  ;;   "Start `robe' after the console."
  ;;   (+robe/start))
  ;; (defadvice inf-ruby (after inf-ruby activate)
  ;;   (+robe/start))
  (set-company-backends 'enh-ruby-mode 'company-robe))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
