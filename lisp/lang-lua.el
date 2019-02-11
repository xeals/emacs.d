;;; lang-lua.el --- Lua language support -*- lexical-binding: t -*-

;;; Commentary:
;; Light.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Functions

(defun +fennel/ident-from-lua (s)
  "Transforms a Lua identified to a Fennel-ish require variables.
Basically exclusively for use in a snippet."
  (s-replace "_" "-" (car (last (s-split "\\." s)))))

;;;
;; Packages

(req-package lua-mode
  :mode "\\.lua$"
  :commands lua-mode
  :general
  (:keymaps 'lua-mode-map
   :states '(normal visual)
   :prefix xeal-localleader-key
   "'" #'lua-show-process-buffer
   "e" '(:ignore t :wk "eval")
   "eb" #'lua-send-buffer
   "ed" #'lua-send-defun
   "el" #'lua-send-current-line
   "er" #'lua-send-region)
  :init
  (set-doc-fn 'lua-mode #'lua-search-documentation)
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

(req-package fennel-mode
  :mode "\\.fnl$"
  :commands fennel-mode
  :general
  (:keymaps 'fennel-mode-map
   :states '(normal visual)
   :prefix xeal-localleader-key
   "'" #'switch-to-lisp
   "e" '(:ignore t :wk "eval")
   "ec" #'lisp-eval-defun
   "er" #'lisp-eval-region
   "f" '(:ignore t :wk "fennel")
   "fr" #'fennel-reload
   "fc" #'fennel-view-compilation)
  :init
  (set-doc-fn 'fennel-mode #'fennel-find-definition))

(provide 'lang-lua)
;;; lang-lua.el ends here
