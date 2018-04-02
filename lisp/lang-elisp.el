;;; lang-elisp.el --- Emacs lisp language support -*- lexical-binding: t -*-

;;; Commentary:
;; Emacs is a """text editor""".

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Functions

(defun +elisp/eval-current-form-sp (&optional arg)
  "Call `eval-last-sexp' after moving out of one level of
parentheses. Will exit any strings and/or comments first.
An optional ARG can be used which is passed to `sp-up-sexp' to move out of more
than one sexp.
Requires smartparens because all movement is done using `sp-up-sexp'."
  (interactive "p")
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (let ((max 10))
        (while (and (> max 0)
                    (sp-point-in-string-or-comment))
          (decf max)
          (sp-up-sexp)))
      (sp-up-sexp arg)
      (call-interactively 'eval-last-sexp))))

(defun +elisp/eval-current-symbol-sp ()
  "Call `eval-last-sexp' on the symbol around point.
Requires smartparens because all movement is done using `sp-forward-symbol'."
  (interactive)
  (require 'smartparens)
  (let ((evil-move-beyond-eol t))
    ;; evil-move-beyond-eol disables the evil advices around eval-last-sexp
    (save-excursion
      (sp-forward-symbol)
      (call-interactively 'eval-last-sexp))))

;;;
;; Packages

(req-package elisp-mode :ensure nil
  ;; :preface
  ;; (defun +use-clisp-indent-function ()
  ;;   (setq-local lisp-indent-function #'common-lisp-indent-function))
  ;; :hook
  ;; (emacs-lisp-mode . +use-clisp-indent-function)
  :general
  (:keymaps 'emacs-lisp-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "e"
            "" '(:ignore t :wk "eval")
            "b" '(eval-buffer :wk "eval buffer")
            "c" '(+elisp/eval-current-form-sp :wk "eval form")
            "d" '(eval-defun :wk "eval defun")
            "r" '(eval-region :wk "eval region")
            "s" '(+elisp/eval-current-symbol-sp :wk "eval symbol"))
  (:keymaps 'emacs-lisp-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "'" #'ielm)
  :config
  (set-prettify-symbols 'emacs-lisp-mode '(("defun"    . ?ƒ)
                                           ("defmacro" . ?μ)
                                           ("defvar"   . ?υ)))

  (set-popup-buffer (rx bos "*ielm*" eos)
                    (rx bos "*Style Warnings*" eos)))

(req-package ielm :ensure nil
  :hook (ielm-mode . rainbow-delimiters-mode))

;; Highlight symbols
(req-package highlight-quoted
  :commands highlight-quoted-mode
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Navigation and documentation
(req-package elisp-slime-nav
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :config
  (set-doc-fn 'emacs-lisp-mode #'elisp-slime-nav-describe-elisp-thing-at-point))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
