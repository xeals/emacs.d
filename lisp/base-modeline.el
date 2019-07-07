;;; base-modeline.el --- Status bar -*- lexical-binding: t -*-

;;; Commentary:
;; Master of the state.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Settings

(use-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-material))

(use-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-buffer-file-name-style 'relative-from-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes nil
        doom-modeline-lsp t)
  (custom-set-faces
   '(doom-modeline-buffer-file ((t (:inherit mode-line-highlight :weight normal))))
   '(doom-modeline-project-dir ((t (:inherit (bold font-lock-variable-name-face)))))
   '(doom-modeline-buffer-path ((t (:inherit font-lock-variable-name-face))))
   ;; FIXME
   '(doom-modeline-buffer-modified ((t (:inherit font-lock-variable-name-face))))
   '(doom-modeline-panel ((t (:inherit match))))
   '(doom-modeline-inactive-bar ((t (:inherit doom-modeline-panel)))))
  :config
  (doom-modeline-def-modeline '+doom/x/modeline
    '(bar modals matches buffer-info buffer-position selection-info)
    '(input-method lsp major-mode vcs checker))
  (defun +doom/setup-modeline ()
    (doom-modeline-set-modeline '+doom/x/modeline 'default))
  (add-hook 'doom-modeline-mode-hook '+doom/setup-modeline)

  (column-number-mode +1))

(use-package feebleline
  :disabled t
  :demand t
  :hook (after-init . feebleline-mode)
  :preface
  (defun feebleline-mode-name () mode-name)

  (defface feebleline-evil-emacs-state
    '((t :inherit (warning bold)))
    "Face used for evil.")
  (defface feebleline-evil-insert-state
    '((t :inherit (error bold)))
    "Face used for evil.")
  (defface feebleline-evil-normal-state
    '((t :inherit (success bold)))
    "Face used for evil.")
  (defface feebleline-evil-replace-state
    '((t :inherit (error bold) :background nil))
    "Face used for evil.")
  (defun feebleline-evil-state ()
    (upcase (substring (format "%s" evil-state) 0 1)))
  (defun feebleline-evil-face ()
    (cond
      ((evil-emacs-state-p) 'feebleline-evil-emacs-state)
      ((evil-insert-state-p) 'feebleline-evil-insert-state)
      ((evil-normal-state-p) 'feebleline-evil-normal-state)
      ((evil-replace-state-p) 'feebleline-evil-replace-state)
      (_ '((t :inherit default)))))
  :init
  (setq feebleline-msg-functions
        '((feebleline-evil-state :fmt "[%s]")
          (feebleline-file-directory :face feebleline-dir-face :post "")
          (feebleline-file-or-buffer-name :face font-lock-keyword-face :post "")
          (feebleline-line-number :post "" :fmt "%5s")
          (feebleline-column-number :pre ":" :fmt "%-2s")
          (feebleline-mode-name :align right)
          (feebleline-git-branch :face feebleline-git-face :align right))))

(provide 'base-modeline)
;;; base-modeline.el ends here
