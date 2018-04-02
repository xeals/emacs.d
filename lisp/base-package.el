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
(autoload 'req-package "req-package" nil nil 'macro)

;;;
;; Functions

(defun +packages-initialise-load-path ()
  "Initialise load path used by packages."
  (dolist (dir (list package-user-dir xeal-el-get-dir))
    (setq load-path (append load-path (directory-files dir t "^[^.]" t))
          custom-theme-load-path (append custom-theme-load-path (directory-files dir t "theme" t)))
    ;; Ensure ELPA org is above built-in org
    (require 'cl)
    (setq load-path (remove-if (lambda (x) (string-match-p "org$" x)) load-path))))

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
    (+packages-initialise-load-path)
    (unless package-archive-contents
      (package-refresh-contents))

    (dolist (package '(use-package el-get req-package))
      (unless (package-installed-p package)
        (package-install package))
      (load (symbol-name package) nil t))

    (setq xeal-packages-init-p t)))

(provide 'base-package)
;;; base-package.el ends here
