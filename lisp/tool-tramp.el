;;; tool-tramp.el --- Transparent Remote Access, Multiple Protocols -*- lexical-binding: t -*-

;;; Commentary:
;; Editing remotely.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(req-package tramp
  :init
  (setq tramp-default-method "ssh"))

(provide 'tool-tramp)
;;; tool-tramp.el ends here
