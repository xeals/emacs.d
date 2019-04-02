;;; lang-clojure.el --- Clojure language support -*- lexical-binding: t -*-

;;; Commentary:
;; Lisp, but in Java!

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Functions

(defun +cider/repl ()
  "Starts a lein repl if inside a project, or otherwise attempts to
connect to an existing nREPL. If a REPL is already active, switches to
the REPL buffer."
  (interactive)
  (condition-case nil
      (call-interactively 'cider-switch-to-repl-buffer)
    (user-error
     (call-interactively 'cider-jack-in))))
;; (if (bound-and-true-p cider-mode)
;;     (cider-switch-to-repl-buffer)
;;   (if (projectile-project-p)
;;       (case major-mode
;;         ('clojure-mode (cider-jack-in))
;;         ('clojurescript-mode (cider-jack-in-clojurescript)))
;;     (case major-mode
;;       ('clojure-mode (call-interactively #'cider-connect))
;;       ('clojurescript-mode (call-interactively #'cider-connect-clojurescript))))))

(defun +cider//eval-in-repl-no-focus (form)
  "Insert FORM in the REPL buffer and eval it."
  (while (string-match "\\`[ \t\n\r]+\\|[ \t\n\r]+\\'" form)
    (setq form (replace-match "" t t form)))
  (with-current-buffer (cider-current-repl-buffer)
    (let ((pt-max (point-max)))
      (goto-char pt-max)
      (insert form)
      (indent-region pt-max (point))
      (cider-repl-return))))

(defun +cider/send-last-sexp-to-repl ()
  "Send last sexp to REPL and evaluate it without changing the focus."
  (interactive)
  (+cider//eval-in-repl-no-focus (cider-last-sexp)))

(defun +cider/send-region-to-repl (start end)
  "Send region to REPL and evaluate it without changing the focus."
  (interactive "r")
  (+cider//eval-in-repl-no-focus
   (buffer-substring-no-properties start end)))

(defun +cider/send-function-to-repl (start end)
  "Send region to REPL and evaluate it without changing the focus."
  (interactive "r")
  (+cider//eval-in-repl-no-focus (cider-defun-at-point)))

;;;
;; Packages

(use-package clojure-mode
  :mode (("\\.clj$"        . clojure-mode)
         ("\\.cljs$"       . clojurescript-mode)
         ("\\.cljc$"       . clojurec-mode)
         ("\\.boot$"       . clojure-mode)
         ;; matches e.g., `#!/usr/bin/env boot'
         ("#!.*boot\\s-*$" . clojure-mode))
  ;; :preface
  ;; (defun +clojure/fancify-symbols (mode)
  ;; "Pretty symbols for Clojure's anonymous functions and sets,
  ;;  like (λ [a] (+ a 5)), ƒ(+ % 5), and ∈{2 4 6}."
  ;; (font-lock-add-keywords mode
  ;;   `(("(\\(fn\\)[\[[:space:]]"
  ;;      (0 (progn (compose-region (match-beginning 1)
  ;;                                (match-end 1) "λ"))))
  ;;     ("(\\(partial\\)[\[[:space:]]"
  ;;      (0 (progn (compose-region (match-beginning 1)
  ;;                                (match-end 1) "Ƥ"))))
  ;;     ("(\\(comp\\)[\[[:space:]]"
  ;;      (0 (progn (compose-region (match-beginning 1)
  ;;                                (match-end 1) "∘"))))
  ;;     ("\\(#\\)("
  ;;      (0 (progn (compose-region (match-beginning 1)
  ;;                                (match-end 1) "ƒ"))))
  ;;     ("\\(#\\){"
  ;;      (0 (progn (compose-region (match-beginning 1)
  ;;                                (match-end 1) "∈")))))))
  :hook
  (clojure-mode . subword-mode)
  :config
  (set-prettify-symbols 'clojure-mode '(("defn"    . ?ƒ)
                                        ("partial" . ?Ƥ)
                                        ("fn"      . ?λ)
                                        ("comp"    . ?∘)
                                        ("->"      . ?→)
                                        ("->>"     . ?»)))
  ;; (dolist (m '(clojure-mode clojurec-mode clojurescript-mode clojurex-mode))
  ;; (+clojure/fancify-symbols m))
  )

(use-package cider
  :hook
  (clojure-mode    . cider-mode)
  (cider-mode      . eldoc-mode)
  (cider-repl-mode . eldoc-mode)
  :commands (cider-mode
             cider-connect
             cider-connect-clojurescript
             cider-jack-in
             cider-jack-in-clojurescript
             cider-switch-to-repl-buffer)
  :general
  (:keymaps 'cider-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "'" #'+cider/repl
   "=" #'cider-format-buffer)
  (:keymaps 'cider-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "e"
   "" '(:ignore t :wk "eval")
   "b" #'cider-eval-buffer
   "d" #'cider-eval-defun-at-point
   "e" #'cider-eval-last-sexp
   "E" #'cider-eval-last-sexp-and-replace
   "m" #'cider-macroexpand-1
   "M" #'cider-macroexpand-all
   "r" #'cider-eval-region)
  (:keymaps 'cider-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "g"
   "" '(:ignore t :wk "goto")
   "C" #'cider-classpath
   "n" #'cider-browse-ns
   "N" #'cider-browse-ns-all
   )
  (:keymaps 'clojure-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "r"
   "" '(:ignore t :wk "repl")
   "TAB" #'cider-repl-switch-to-other
   "b" #'cider-load-buffer
   "c" #'+cider/send-last-sexp-to-repl
   "d" #'+cider/send-function-to-repl
   "i" #'cider-jack-in
   "I" #'cider-jack-in-clojurescript
   "n" #'cider-connect
   "Q" #'cider-quit
   "r" #'+cider/send-region-to-repl
   "x" #'cider-refresh)
  (:keymaps 'clojure-repl-mode-map
   :states 'normal
   "C-j" #'cider-repl-next-input
   "C-k" #'cider-repl-previous-input)
  :init
  (setq cider-repl-use-clojure-font-lock t)
  (set-doc-fn 'cider-mode #'cider-doc))

(provide 'lang-clojure)
;;; lang-clojure.el ends here
