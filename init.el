;;; init.el --- Main init file -*- lexical-binding: t no-byte-compile: t; -*-

;;; Commentary:
;;; The init file that loads all the components.
;;; Heavily inspired by github.com/terlar

;;; Code:

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

(require 'lang-clojure)
(require 'lang-elisp)
(require 'lang-ess)
(require 'lang-go)
(require 'lang-haskell)
(require 'lang-markdown)
(require 'lang-markup)
(require 'lang-org)
(require 'lang-rust)
(require 'lang-crystal)
(require 'lang-nim)

(unless noninteractive
  (require 'bindings)
  ;; (require 'commands)
  (require 'theme))

(req-package-finish)

;;; init.el ends here
