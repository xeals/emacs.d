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

(provide 'base-keybinds)
;;; base-keybinds.el ends here
