;;; base-projects.el --- Project configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Project management.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Packages

(use-package projectile
  :demand t
  :commands (projectile-ack
             projectile-ag
             projectile-compile-project
             projectile-dired
             projectile-find-dir
             projectile-find-file
             projectile-find-tag
             projectile-test-project
             projectile-grep
             projectile-invalidate-cache
             projectile-kill-buffers
             projectile-multi-occur
             projectile-project-p
             projectile-project-root
             projectile-recentf
             projectile-regenerate-tags
             projectile-replace
             projectile-replace-regexp
             projectile-run-async-shell-command-in-root
             projectile-run-shell-command-in-root
             projectile-switch-project
             projectile-switch-to-buffer
             projectile-vc)
  :init
  (setq projectile-sort-order #'recentf
        projectile-cache-file (x/cache "projectile.cache")
        projectile-known-projects-file (x/cache "projectile-bookmarks.eld")
        projectile-globallyy-ignored-file-suffixes
        '(".elc" ".pyc" ".o" ".hi" ".class" ".cache")
        projectile-ignored-projects (list xeal-data-dir)
        )
  :config
  (projectile-mode))

(provide 'base-projects)
;;; base-projects.el ends here
