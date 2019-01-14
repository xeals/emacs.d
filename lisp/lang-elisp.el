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

;; Fix the indentation on keyword lists.
;; Before:
;;   (:foo bar
;;         :baz qux)
;; After:
;;   (:foo bar
;;    :baz qux)
;; From
;; https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L20-L94
(defun +elisp/indent-function (indent-point state)
  "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.

INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.

If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:

* `defun', meaning indent `defun'-style
  \(this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);

* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;

* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.

This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
  (let ((normal-indent (current-column))
        (orig-point (point)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (cond
     ;; car of form doesn't seem to be a symbol, or is a keyword
     ((and (elt state 2)
           (or (not (looking-at "\\sw\\|\\s_"))
               (looking-at ":")))
      (if (not (> (save-excursion (forward-line 1) (point))
                  calculate-lisp-indent-last-sexp))
          (progn (goto-char calculate-lisp-indent-last-sexp)
                 (beginning-of-line)
                 (parse-partial-sexp (point)
                                     calculate-lisp-indent-last-sexp 0 t)))
      ;; Indent under the list or under the first sexp on the same
      ;; line as calculate-lisp-indent-last-sexp.  Note that first
      ;; thing on that line has to be complete sexp since we are
      ;; inside the innermost containing sexp.
      (backward-prefix-chars)
      (current-column))
     ((and (save-excursion
             (goto-char indent-point)
             (skip-syntax-forward " ")
             (not (looking-at ":")))
           (save-excursion
             (goto-char orig-point)
             (looking-at ":")))
      (save-excursion
        (goto-char (+ 2 (elt state 1)))
        (current-column)))
     (t
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (function-get (intern-soft function)
                                       'lisp-indent-function)
                         (get (intern-soft function) 'lisp-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        (string-match "\\`def" function)))
               (lisp-indent-defform state indent-point))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method indent-point state))))))))

;;;
;; Packages

(req-package elisp-mode :ensure nil
  :preface
  (defun +elisp/use-indent-function ()
    (setq-local lisp-indent-function #'+elisp/indent-function))
  :hook
  (emacs-lisp-mode . +elisp/use-indent-function)
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
  (:keymaps 'emacs-lisp-mode-map :major-modes t
   :states '(normal)
   "<return>"   #'+elisp/eval-current-form-sp
   "<S-return>" #'eval-defun)
  :config
  (set-prettify-symbols 'emacs-lisp-mode '(("defun"    . ?ƒ)
                                           ("defmacro" . ?μ)
                                           ("defvar"   . ?υ))))

(req-package ielm :ensure nil
  :hook (ielm-mode . rainbow-delimiters-mode))

;; Highlight symbols
(req-package highlight-quoted
  :commands highlight-quoted-mode
  :hook (emacs-lisp-mode . highlight-quoted-mode))

;; Navigation and documentation
(req-package elisp-slime-nav
  :hook (emacs-lisp-mode . turn-on-elisp-slime-nav-mode)
  :init
  (set-doc-fn 'emacs-lisp-mode #'elisp-slime-nav-describe-elisp-thing-at-point)
  (set-doc-fn 'inferior-emacs-lisp-mode #'elisp-slime-nav-describe-elisp-thing-at-point))

(provide 'lang-elisp)
;;; lang-elisp.el ends here
