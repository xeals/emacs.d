;;; lang-crystal.el --- Crystal language support -*- lexical-binding: t -*-

;;; Commentary:
;; Ruby, but compiled.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(req-package crystal-mode
  :mode "\\.cr$"
  :hook (crystal-mode . eglot-ensure)
  :general
  (:keymaps 'crystal-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "g" '(:ignore t :wk "goto")
   "gb" #'crystal-backward-sexp
   "gf" #'crystal-forward-sexp
   "gn" #'crystal-end-of-block
   "gp" #'crystal-beginning-of-block)
  :init
  (set-doc-fn 'crystal-mode #'crystal-def-jump))

(req-package slim-mode
  :mode "\\.slim$"
  :mode "\\.slang$")

(provide 'lang-crystal)
;;; lang-crystal.el ends here
