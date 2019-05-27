;;; lang-python.el --- Python language support -*- lexical-binding: t -*-

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-feature python-mode
  :hook (python-mode . flycheck-mode))

(use-package jedi
  :preface
  (defun jedi:setup ()
    (setq jedi:setup-keys t
          jedi:complete-on-dot t))
  :hook
  (python-mode . jedi-mode)
  (jedi-mode . jedi:setup)
  :general
  (:keymaps 'jedi-mode-map
   :states '(normal emacs)
   "K" #'jedi:show-doc)
  :init
  ;; broken for some reason
  (set-doc-fn 'python-mode #'jedi:show-doc))

(use-package company-jedi
  :hook (jedi-mode . company-mode)
  :init
  (set-company-backends 'python-mode 'company-jedi))

(use-package py-autopep8
  :hook (python-mode . py-autopep8-enable-on-save))

(use-package python-docstring
  :hook (python-mode . python-docstring-mode))

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "-i")

(provide 'lang-python)

;;; lang-python.el ends here
