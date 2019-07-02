;;; base-keybinds.el --- Key binding support -*- lexical-binding: t -*-

;;; Commentary:
;; Touch me, feel me all over.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Configuration

;; Leader keys
(defvar xeal-leader-key "SPC"
  "The leader prefix key, for global commands.")

(defvar xeal-non-normal-leader-key "C-SPC"
  "The leader prefix key, for global commands, outside normal mode.")

(defvar xeal-localleader-key ","
  "The localleader prefix key, for major-mode specific commands.")

;;;
;; Packages

(use-package general
  :demand t)

;; https://github.com/noctuid/general.el/issues/126

(general-create-definer xeal--default-leader
  :states '(normal visual)
  :prefix xeal-leader-key
  :keymaps 'override)

(general-create-definer xeal-global-leader
  :states general-non-normal-states
  :prefix xeal-non-normal-leader-key
  :keymaps 'override)

(defmacro general-leader (&rest args)
  "Define for both default leader and global leader."
  (declare (indent defun))
  `(progn
     (xeal--default-leader
       ,@args)
     (xeal-global-leader
       ,@args)))

;; Remove conflicting leader keys
(after! evil
  (general-define-key
   :unbind t
   :keymaps '(normal visual motion)
   xeal-leader-key xeal-localleader-key)
  (general-define-key
   :unbind t
   :keymaps general-non-normal-states
   xeal-non-normal-leader-key))

(use-package which-key
  :commands
  (which-key-mode
   which-key-key-order-alpha)
  :defer 1
  :init
  (setq which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-idle-delay 0.4
        which-key-echo-keystrokes 0.02
        echo-keystrokes 0.02)
  :config
  (push '(("SPC" . nil) . ("␣" . nil)) which-key-replacement-alist)
  (push '(("TAB" . nil) . ("↹" . nil)) which-key-replacement-alist)
  (push '(("RET" . nil) . ("⏎" . nil)) which-key-replacement-alist)
  (push '(("DEL" . nil) . ("⌫" . nil)) which-key-replacement-alist)
  (push '(("deletechar" . nil) . ("⌦" . nil)) which-key-replacement-alist)

  (which-key-setup-side-window-bottom)

  (which-key-mode 1))

;;;
;; Universal, non-nuclear escape

;; `keyboard-quit' is too much of a nuclear option. I wanted an ESC/C-g to
;; do-what-I-mean. It serves four purposes (in order):
;;
;; 1. Quit active states; e.g. highlights, searches, snippets, iedit,
;;    multiple-cursors, recording macros, etc.
;; 2. Close popup windows remotely (if it is allowed to)
;; 3. Refresh buffer indicators, like git-gutter and flycheck
;; 4. Or fall back to `keyboard-quit'
;;
;; And it should do these things incrementally, rather than all at once. And it
;; shouldn't interfere with recording macros or the minibuffer. This may require
;; you press ESC/C-g two or three times on some occasions to reach
;; `keyboard-quit', but this is much more intuitive.

;; Borrowed from doom-emacs.

(defvar escape-hook nil
  "A hook run after C-g or ESC in normal mode is pressed. Both trigger `xeal/escape'.

If any hook returns non-nil, all hooks after it are ignored.")

(defun xeal/escape ()
  "Run `escape-hook'."
  (interactive)
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Fallback
        ((keyboard-quit))))

(global-set-key [remap keyboard-quit] #'xeal/escape)

(provide 'base-keybinds)
;;; base-keybinds.el ends here
