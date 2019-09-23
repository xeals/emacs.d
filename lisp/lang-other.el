;;; lang-markup.el --- Common markup language support -*- lexical-binding: t -*-

;;; Commentary:
;; Stuff that doesn't justify an individual file because it's a two-liner.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package yaml-mode
  :mode "\\.ya?ml$")

(use-package toml-mode
  :mode "\\.toml$")

(use-package fish-mode
  :mode "\\.fish$")

(provide 'lang-markup)
;;; lang-markup.el ends here
