;;; lang-ess.el --- ESS language support -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs Speaks Statistics -- or only R.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(use-package ess
  :commands (R stata julia SAS)
  :mode (("/R/.*\\.q$"      . R-mode)
         ("\\.[rR]$"        . R-mode)
         ("\\.[rR]profile$" . R-mode))
  :hook
  (ess-mode . run-prog-mode-hooks)
  (ess-mode . company-mode)
  ;; (org-mode . +ess/setup-babel)
  :preface
  (defun +ess/start-repl ()
    (interactive)
    (cond
     ((string= "S" ess-language) (call-interactively #'R))
     ((string= "STA" ess-language) (call-interactively #'stata))
     ((string= "SAS" ess-language) (call-interactively #'SAS))))
  (defun +ess/setup-babel ()
    (add-to-list 'org-babel-load-languages
                 '((R . t))))
  :general
  (:keymaps 'ess-mode-map :major-modes t
   :states 'insert
   "<S-return>" #'ess-eval-line)
  (:keymaps 'ess-mode-map :major-modes t
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "'" '(+ess/start-repl :wk "start repl"))
  :config
  (after! ess-site
    ;; Follow Hadley Wickham's R style guide
    (setq ess-offset-continued 0
          ess-expression-offset 2
          ess-nuke-trailing-whitespace-p t
          ess-default-style 'DEFAULT)))

(provide 'lang-ess)
;;; lang-ess.el ends here
