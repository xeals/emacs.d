;;; lang-markup.el --- Common markup language support -*- lexical-binding: t -*-

;;; Commentary:
;; Stuff that doesn't justify an individual file because it's a two-liner.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package yaml-mode
  :mode "\\.ya?ml$")

(req-package toml-mode
  :mode "\\.toml$")

(provide 'lang-markup)
;;; lang-markup.el ends here
