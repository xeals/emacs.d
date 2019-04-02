;;; feature-evil.el --- Extensible vi layer -*- lexical-binding: t -*-

;;; Commentary:
;; Modal editing was a mistake.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Packages

(use-package evil
  :demand t
  :general
  (:keymaps 'insert
   "C-g" #'normal-state-or-keyboard-quit)
  :init
  (setq evil-want-C-d-scroll t
        evil-want-C-i-jump t
        evil-want-C-u-scroll t
        evil-want-C-w-delete t
        evil-want-C-w-in-emacs-state t
        evil-want-integration t
        evil-magic t
        evil-indent-convert-tabs t
        evil-ex-search-persistent-highlight nil
        evil-ex-search-vim-style-regexp t
        evil-ex-visual-char-range t
        evil-mode-line-format 'nil
        shift-select-mode nil
        evil-symbol-word-search t)
  :config
  ;; disable selection adding to clipboard
  (fset 'evil-visual-update-x-selection 'ignore)

  (evil-select-search-module 'evil-search-module 'evil-search)

  (evil-mode 1))

;; Additional objects
(use-package evil-args
  :commands (evil-jump-out-args evil-inner-arg evil-outer-arg
                                evil-forward-arg evil-backward-arg)
  :general
  (:keymaps 'inner
   "a" #'evil-inner-arg)
  (:keymaps 'outer
   "a" #'evil-outer-arg))

;; Better lisping
(use-package evil-cleverparens
  :disabled t
  :hook
  ((lisp-mode emacs-lisp-mode) . evil-cleverparens-mode))

;; Better escaping
(use-package evil-escape
  :commands evil-escape-mode
  :hook
  (after-init . evil-escape-mode)
  :init
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.1))

;; Visual feedback
(use-package evil-goggles
  :commands evil-goggles-mode
  :config
  (evil-goggles-mode)
  (evil-goggles-use-magit-faces))

;; Align all the things
(use-package evil-lion
  :commands (evil-lion-left evil-lion-right)
  :general
  (:keymaps 'visual
   "gl" #'evil-lion-left
   "gL" #'evil-lion-right)
  :config
  (evil-lion-mode 1))

;; Magit integration
(use-package evil-magit
  :demand t
  :after (evil magit))

;; Improved % matching
(use-package evil-matchit
  :commands (evilmi-jump-items evilmi-text-object global-evil-matchit-mode)
  :general
  ([remap evil-jump-item] #'evilmi-jump-items)
  (:keymaps '(inner outer)
   "%" #'evilmi-text-object)
  :config
  (global-evil-matchit-mode 1))

;; Commenting keys
(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator evilnc-comment-or-uncomment-lines))

;; Org integration
(use-package evil-org
  :commands (evil-org-mode evil-org-recalculate-clocs)
  :after (org evil)
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . evil-org-set-key-theme))

;; Easily add surrounding delimiters
(use-package evil-surround
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region
             evil-Surround-region)
  :general
  (:keymaps 'visual
   "s" #'evil-surround-region)
  (:keymaps 'operator
   "s" #'evil-surround-edit
   "S" #'evil-Surround-edit)
  :config
  (global-evil-surround-mode 1))

;; Allow visual selections to be searched
(use-package evil-visualstar
  :commands (global-evil-visualstar-mode
             evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :general
  (:keymaps 'visual
   "*" #'evil-visualstar/begin-search-forward
   "#" #'evil-visualstar/begin-search-backward))

(defun normal-state-or-keyboard-quit ()
  "If in evil insert state, force normal state; else run `keyboard-quit'."
  (interactive)
  (if (and evil-mode (eq evil-state 'insert))
      (evil-force-normal-state)
    (keyboard-quit)))

(provide 'feature-evil)
;;; feature-evil.el ends here
