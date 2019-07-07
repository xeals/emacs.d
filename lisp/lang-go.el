;;; lang-go.el --- Go language support -*- lexical-binding: t -*-

;;; Commentary:
;; Web servers and stuff.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(use-package go-mode
  :mode "\\.go$"
  :hook
  (go-mode . flycheck-mode)
  (go-mode . +go-mode-setup)
  :preface
  (defun +go-mode-setup ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t))
  :general
  (:keymaps 'go-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "d" #'godef-jump-other-window
   "g" '(:ignore t :wk "goto")
   "ga" #'go-goto-arguments
   "gd" #'go-goto-docstring
   "gf" #'go-goto-function
   "gi" #'go-goto-imports
   "gm" #'go-goto-method-receiver
   "gn" #'go-goto-function-name
   "gr" #'go-goto-return-values
   "i" '(:ignore t :wk "imports")
   "ia" #'go-import-add
   "ig" #'go-goto-imports
   "ir" #'go-remove-unused-imports)
  :init
  (setq flycheck-go-build-install-deps nil
        godoc-at-point-function #'godoc-gogetdoc
        godoc-and-godef-command "go doc"
        gofmt-command "goimports")
  :config
  (set-doc-fn 'go-mode #'godoc-at-point)
  (set-prettify-symbols 'go-mode
                        '(("func" . ?ƒ)
                          (":="   . ?←)))

  ;; fix up godoc-mode to be consistent with literally every other documentation mode
  (add-hook 'godoc-mode-hook 'help-mode))

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(use-package go-guru
  :commands (go-guru-describe
             go-guru-freevars
             go-guru-implements
             go-guru-peers
             go-guru-referrers
             go-guru-definition
             go-guru-pointsto
             go-guru-callstack
             go-guru-whicherrs
             go-guru-callers
             go-guru-callees
             go-guru-expand-region)
  :general
  (:keymaps 'go-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "g"
   "" '(:ignore t :wk "guru")
   "<" #'go-guru-callers
   ">" #'go-guru-callees
   "c" #'go-guru-peers
   "d" #'go-guru-describe
   "f" #'go-guru-freevars
   "i" #'go-guru-implements
   "p" #'go-guru-pointsto
   "r" #'go-guru-referrers))

(use-package gorepl-mode
  :if (executable-find "gore")
  :commands (gorepl-run gorepl-run-load-current-file)
  :hook (go-mode . gorepl-mode)
  :general
  (:keymaps 'go-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "'" #'gorepl-run)
  (:keymaps 'gorepl-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "r"
   "" '(:ignore t :wk "repl")
   "f" #'gorepl-run-load-current-file
   "h" '(gorepl-hydra/body :wk "hydra")
   "l" #'gorepl-eval-line
   "r" #'gorepl-eval-region))

(use-package company-go
  :commands company-go
  :init
  (setq company-go-gocode-command "gocode")
  (when (executable-find company-go-gocode-command)
    (set-company-backends 'go-mode 'company-go)))

(use-package flycheck-gometalinter
  :if (executable-find "gometalinter")
  :commands flycheck-gometalinter-setup
  :hook (flycheck-mode . flycheck-gometalinter-setup)
  :init
  (setq flycheck-gometalinter-vendor t
        flycheck-gometalinter-disabled-linters '("gocyclo")))

(provide 'lang-go)
;;; lang-go.el ends here
