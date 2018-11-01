;;; feature-version-control.el --- Git configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Version controller.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(req-package magit
  :defer 2
  :general
  (:keymaps 'with-editor-mode
            :definer 'minor-mode
            :states 'normal
            :prefix xeal-localleader-key
            xeal-localleader-key #'with-editor-finish
            "k" #'with-editor-cancel)
  :config
  (global-magit-file-mode 1))

(req-package gitconfig-mode
  :mode "/\\.?git/?config$"
  :mode "/\\.gitmodules$")

(req-package gitignore-mode
  :mode "/\\.gitignore$")

(req-package git-gutter-fringe
  :commands git-gutter-mode
  :preface
  (defun +version-control/git-gutter-maybe ()
    "Enable `git-gutter-mode' in non-remote buffers."
    (when (and (buffer-file-name)
               (not (file-remote-p (buffer-file-name))))
      (git-gutter-mode 1)))
  :hook
  (text-mode . +version-control/git-gutter-maybe)
  (prog-mode . +version-control/git-gutter-maybe)
  (conf-mode . +version-control/git-gutter-maybe)
  :init
  (setq git-gutter-fr:side         #'right-fringe
        git-gutter-fr:window-width 1
        git-gutter:unchanged-sign  " "
        git-gutter:modified-sign   "~"
        git-gutter:added-sign      "+"
        git-gutter:deleted-sign    "*")
  :config
  (fringe-helper-define 'git-gutter-fr:added nil
    "..X...."
    "..X...."
    "XXXXX.."
    "..X...."
    "..X....")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "......."
    "......."
    "XXXXX.."
    "......."
    ".......")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "..X...."
    ".XXX..."
    "XX.XX.."
    ".XXX..."
    "..X...."))

(provide 'feature-version-control)
;;; feature-version-control.el ends here
