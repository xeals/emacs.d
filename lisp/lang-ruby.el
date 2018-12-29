;;; lang-ruby.el --- Ruby support -*- lexical-binding: t -*-

;;; Commentary:
;; Gems.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Packages

(req-package ruby-mode
  :hook
  (ruby-mode . eglot-ensure)
  :init
  (set-doc-fn 'ruby-mode #'eglot-help-at-point))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
