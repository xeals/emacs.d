;;; feature-syntax-checker.el --- Syntax checking -*- lexical-binding: t -*-

;;; Commentary:
;; Maybe I'm actually American.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

;; pkg-info doesn't get autoloaded when `flycheck-version' needs it.
(autoload 'pkg-info-version-info "pkg-info")

;;;
;; Packages

(use-package flycheck
  :demand t
  :commands (flycheck-list-errors flycheck-buffer flycheck-add-next-checker)
  :general
  (:keymaps 'flycheck-error-list-mode-map
   :states 'normal
   "C-n" #'flycheck-error-list-next-error
   "C-p" #'flycheck-error-list-previous-error
   "j"   #'flycheck-error-list-next-error
   "k"   #'flycheck-error-list-previous-error
   "RET" #'flycheck-error-list-goto-error
   "q"   #'quit-window)
  :init
  (setq flycheck-check-syntax-automatically '(save idle-change new-line mode-enabled)))

(use-package flycheck-posframe
  :after flycheck
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (flycheck-posframe-configure-pretty-defaults))

(use-package flymake-diagnostic-at-point
  :after flymake
  :hook (flymake-mode . flymake-diagnostic-at-point-mode))

(use-package flymake-posframe
  :disabled t
  :straight (:type git
             :repo "https://git.owari.cc/xeals/flymake-posframe")
  :hook
  (flymake-mode . flymake-posframe-mode)
  :init
  (setq flymake-posframe-display-delay 0.5
        flymake-posframe-warning-prefix "⚠ "
        flymake-posframe-error-prefix "❌ ")
  :config
  (set-face-attribute 'flymake-posframe-warning-face
                      nil :inherit 'warning)
  (set-face-attribute 'flymake-posframe-error-face
                      nil :inherit 'error))

(provide 'feature-syntax-checker)
;;; feature-syntax-checker.el ends here
