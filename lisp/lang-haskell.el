;;; lang-haskell.el --- Haskell language support -*- lexical-binding: t -*-

;;; Commentary:
;; Functionally functional.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package)
  (require 'base-lib))

;;;
;; Packages

(req-package haskell-mode
  :mode
  "\\.[gh]s$"
  "\\.hsc$"
  ("\\.l[gh]s$" . literate-haskell-mode)
  :hook
  (haskell-mode . flycheck-mode)
  (haskell-mode . company-mode)
  (haskell-mode . +haskell-disable-electric-indent)
  ;; (haskell-mode . rainbow-identifiers-mode)
  :interpreter
  "runghc"
  "runhaskell"
  :preface
  (defun +haskell-disable-electric-indent ()
    (if (fboundp 'electric-indent-local-mode)
        (electric-indent-local-mode -1)))
  :general
  (:keymaps 'haskell-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "" '(:ignore t)
            "=" #'haskell-mode-stylish-buffer
            "c" '(:ignore t :wk "compile")
            "cc" #'haskell-compile
            "ca" #'haskell-process-cabal
            "cb" #'haskell-process-cabal-build
            "h" #'hoogle)
  (:keymaps 'haskell-cabal-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "d"  #'haskell-cabal-add-dependency
            "b"  #'haskell-cabal-goto-benchmark-section
            "e"  #'haskell-cabal-goto-executable-section
            "t"  #'haskell-cabal-goto-test-suite-section
            "m"  #'haskell-cabal-goto-exposed-modules
            "l"  #'haskell-cabal-goto-library-section
            "n"  #'haskell-cabal-next-subsection
            "p"  #'haskell-cabal-previous-subsection
            "sc" #'haskell-interactive-mode-clear
            "ss" #'spacemacs/haskell-interactive-bring
            "sS" #'haskell-interactive-switch
            "N"  #'haskell-cabal-next-section
            "P"  #'haskell-cabal-previous-section
            "f"  #'haskell-cabal-find-or-create-source-file)
  :init
  (maybe-push-exec-path (expand-file-name "~/.local/share/cabal/bin"))
  (autoload 'haskell-completions-completion-at-point "haskell-completions")
  (autoload 'haskell-doc-current-info "haskell-doc")
  (setq
   haskell-notify-p t
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-stylish-on-save t
   haskell-font-lock-symbols t
   haskell-process-type 'cabal-repl))

(req-package ghc)

(req-package company-ghc
  ;; :general
  ;; (:keymaps haskell-ghc-mode-map
  ;;           :states '(normal visual operator)
  ;;           :prefix xeal-localleader-key
  ;;           )
  :hook
  (haskell-mode . ghc-init)
  :config
  (set-doc-fn 'haskell-mode #'hoogle)
  ;; (set-company-backends 'haskell-mode 'company-ghc)
  (add-to-list 'company-backends 'company-ghc)
  )

(req-package company-cabal
  :hook (haskell-cabal-mode . company-mode)
  :config
  (set-company-backends 'haskell-cabal-mode 'company-cabal))

(req-package company-ghci
  :disabled t
  :config
  (set-company-backends 'haskell-mode 'company-ghci)
  (push 'company-ghci company-backends)
  ;; (add-to-list 'company-backends-haskell-mode
  ;;              '(company-ghci company-dabbrev-code company-yasnippet))
  )

(req-package dante
  :disabled t
  :hook
  (haskell-mode . dante-mode)
  (dante-mode . +dante-use-hlint)
  :preface
  (defun +dante-use-hlint ()
    (flycheck-add-next-checker 'haskell-dante
                               '(warning . haskell-hlint))))

(req-package intero
  :disabled t
  :hook
  (haskell-mode . intero-mode)
  :general
  (:keymaps 'intero-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "r" '(:ignore t :wk "repl")
            "rb" #'intero-repl-load
            "i" '(:ignore t :wk "intero")
            "ic" #'intero-cd
            "id" #'intero-devel-reload
            "ik" #'intero-destroy
            "il" #'intero-list-buffers
            "ir" #'intero-restart
            "it" #'intero-targets)
  :config
  (set-company-backends 'haskell-mode 'company-intero)
  (set-doc-fn 'intero-mode #'intero-info))

(provide 'lang-haskell)
;;; lang-haskell.el ends here
