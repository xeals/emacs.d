;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.
;;; Heavily inspired by github.com/terlar

;;; Code:

;;;
;; Startup optimisation

(let ((normal-gc-cons-threshold 800000)
      (normal-gc-cons-percentage 0.1)
      (normal-file-name-handler-alist file-name-handler-alist)
      (init-gc-cons-threshold 402653184)
      (init-gc-cons-percentage 0.6))
  (setq gc-cons-threshold init-gc-cons-threshold
        gc-cons-percentage init-gc-cons-percentage
        file-name-handler-alist nil)
  (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold normal-gc-cons-threshold
                                         gc-cons-percentage normal-gc-cons-percentage
                                         file-name-handler-alist normal-file-name-handler-alist))))

;; Startup time
(add-hook 'emacs-startup-hook
          (lambda () (message "Loaded Emacs in %.03fs"
                         (float-time (time-subtract after-init-time before-init-time)))))

;; Disable GUI components early
(when window-system
  (tool-bar-mode 0)
  (scroll-bar-mode 0)
  (menu-bar-mode 0)
  ;; Tooltips in echo area
  (tooltip-mode 0))


;; Quiet startup
(advice-add #'display-startup-echo-area-message :override #'ignore)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;;;
;; Load

(eval-and-compile
  (push (concat user-emacs-directory "lisp") load-path)
  (push (concat user-emacs-directory "site-lisp") load-path))

;;;
;; Base

;; Calls (package-initialize)
(require 'base)

;;;
;; Features

(require 'feature-evil)
(require 'feature-hydra)
(require 'feature-snippets)
(require 'feature-spellcheck)
(require 'feature-syntax-checker)
(require 'feature-version-control)
(require 'feature-preview)
(require 'feature-reference)

;;;
;; Completion

(require 'completion-ivy)
(require 'completion-company)
(require 'completion-lsp)

;;;
;; Tools

(require 'tool-dired)
(require 'tool-quick-find)
(require 'tool-tramp)
(require 'tool-pairing)
(require 'tool-ledger)

;;;
;; Language support

(require 'lang-c)
(require 'lang-clojure)
(require 'lang-crystal)
(require 'lang-elisp)
(require 'lang-ess)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-markdown)
(require 'lang-markup)
(require 'lang-nim)
(require 'lang-org)
(require 'lang-ruby)
(require 'lang-rust)

(unless noninteractive
  (require 'bindings)
  (require 'theme))

(req-package-finish)

;;; init.el ends here
