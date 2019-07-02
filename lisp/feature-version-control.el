;;; feature-version-control.el --- Git configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Version controller.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(use-package magit
  :defer 2
  :general
  (:keymaps 'with-editor-mode
   :definer 'minor-mode
   :states 'normal
   :prefix xeal-localleader-key
   xeal-localleader-key #'with-editor-finish
   "k" #'with-editor-cancel)
  :config
  ;; (set-popup-buffer (rx bos "magit" (one-or-more anything) eos))
  (global-magit-file-mode 1))

(use-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(use-package gitignore-mode
  :mode "/\\.gitignore$")

(use-package git-gutter-fringe
  :commands git-gutter-mode
  :preface
  (defun +version-control/git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (require 'git-gutter-fringe)
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode 1)))
  :hook
  (text-mode . +version-control/git-gutter-maybe)
  (prog-mode . +version-control/git-gutter-maybe)
  (conf-mode . +version-control/git-gutter-maybe)
  :init
  (if (fboundp 'fringe-mode)
      (fringe-mode '4)
    (setq-default left-fringe-width 4))
  (setq-default fringes-outside-margins t)
  (setq git-gutter-fr:side         #'left-fringe
        git-gutter-fr:window-width 1
        git-gutter:unchanged-sign  nil
        git-gutter:modified-sign   "~"
        git-gutter:added-sign      "+"
        git-gutter:deleted-sign    "*")
  :config
  ;; Borrowed from doom-emacs.
  (defun +version-control/git-gutter-update (&rest _)
    "Refresh git-gutter on ESC. Return nil to prevent shadowing
other `escape-hook' hooks."
    (when git-gutter-mode
      (ignore (git-gutter))))
  (add-hook 'escape-hook #'+version-control/git-gutter-update t)
  (define-fringe-bitmap 'git-gutter-fr:added [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224]
    nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240]
    nil nil 'bottom)
  (after! flycheck
    (setq flycheck-indication-mode 'right-fringe)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))

(provide 'feature-version-control)
;;; feature-version-control.el ends here
