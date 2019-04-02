;;; base-package.el --- Package configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Sharing is caring.
;; Largely inspired by github.com/terlar.

;;; Code:

(eval-when-compile
  (require 'base-vars)

  (require 'package)
  (require 'tls))

(defvar xeal-packages-init-p nil
  "Non-nil if the package system has been initialised.
This will be nil if you have byte-compiled your configuration.")

(defvar xeal-el-get-dir (expand-file-name "el-get" xeal-packages-dir))
(defvar xeal-el-get-recipes (expand-file-name "recipes" user-emacs-directory))

;;;
;; Settings

(setq-default
 load-prefer-newer noninteractive
 package--init-file-ensured t
 package-enable-at-startup nil
 package-user-dir (expand-file-name "elpa" xeal-packages-dir)
 package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                    ("melpa"        . "https://melpa.org/packages/")
                    ("melpa-stable" . "https://stable.melpa.org/packages/")
                    ("org"          . "https://orgmode.org/elpa/"))

 gnutls-verify-error t
 tls-checktrust gnutls-verify-error

 ;; use-package
 use-package-always-defer t
 use-package-always-ensure t
 use-package-debug nil
 use-package-expand-minimally (eval-when-compile (not xeal-debug-mode))
 use-package-minimum-reported-time (if xeal-debug-mode 0 0.1)
 use-package-verbose xeal-debug-mode

 ;; el-get
 el-get-dir xeal-el-get-dir
 el-get-recipe-path `(,xeal-el-get-recipes)
 el-get-status-file (expand-file-name ".status.el" xeal-el-get-dir)
 el-get-autoload-file (expand-file-name ".loaddefs.el" xeal-el-get-dir)

 ;; use-package
 straight-use-package-by-default t

 ;; req-package
 req-package-log-level (if (and (not noninteractive) xeal-debug-mode)
                           'debug
                         'info)

 byte-compile-dynamic nil
 byte-compile-verbose xeal-debug-mode
 byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Prevent packages from being saved to custom file.
(defun package--save-selected-packages (&optional value)
  "Set and (don't!) save `package-selected-packages' to VALUE."
  (when value
    (setq package-selected-packages value))
  (unless after-init-time
    (add-hook 'after-init-hook #'package--save-selected-packages)))

;;;
;; Macros

(autoload 'use-package "use-package" nil nil 'macro)
;; (autoload 'req-package "req-package" nil nil 'macro)

(defmacro use-feature (name &rest args)
  "Like `use-package', but with `straight-use-package-by-default' disabled."
  (declare (indent defun))
  `(use-package ,name
     :straight nil
     ,@args))

;;;
;; Functions

(defun +straight//force-load-org ()
  "Forces loading straight.el's org-mode before the built-in one.

See https://github.com/raxod502/radian/blob/b32ab33c8b60f9b2f8f7f56b02ebe1cb5b45dd80/emacs/radian.el#L358"
  (use-package git)

  (defun org-git-version()
    "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (git-run "describe"
                "--match=release\*"
                "--abbrev=6"
                "HEAD"))))

  (defun org-release ()
    "The release version of org-mode.
Inserted by installing Org mode or when a release is made."
    (require 'git)
    (let ((git-repo (expand-file-name
                     "straight/repos/org/" user-emacs-directory)))
      (string-trim
       (string-remove-prefix
        "release_"
        (git-run "describe"
                 "--match=release\*"
                 "--abbrev=0"
                 "HEAD")))))

  (provide 'org-version)

  ;; Our real configuration for Org comes much later. Doing this now
  ;; means that if any packages that are installed in the meantime
  ;; depend on Org, they will not accidentally cause the Emacs-provided
  ;; (outdated and duplicated) version of Org to be loaded before the
  ;; real one is registered.
  (straight-use-package 'org))

(defun +straight//bootstrap ()
  "Bootstrap `straight.el'."
  ;; FIXME Make this use some package-dir [1].
  ;; [1]: https://github.com/raxod502/straight.el/pull/300
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun +packages-initialise (&optional force-p)
  "Initialise installed packages and ensure they are installed.
When FORCE-P is provided it will run no matter the preconditions.
When base.el is compiled ,this function will be avoided to speed up startup."
  (when (or (not xeal-packages-init-p) force-p)
    (setq package-activated-list nil)

    ;; Ensure folders exits
    (dolist (dir (list xeal-cache-dir xeal-data-dir package-user-dir xeal-el-get-dir))
      (unless (file-directory-p dir)
        (make-directory dir t)))

    (package-initialize t)
    (unless package-archive-contents
      (package-refresh-contents))

    ;; Bootstrap
    (+straight//bootstrap)
    (straight-use-package 'use-package)
    (+straight//force-load-org)

    (setq xeal-packages-init-p t)))

(provide 'base-package)
;;; base-package.el ends here
