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

(defvar xeal-localleader-key ","
  "The localleader prefix key, for major-mode specific commands.")

;;;
;; Packages

(req-package general :force t :demand t)

(general-create-definer
 general-leader
 :states '(normal motion visual motion emacs operator)
 :prefix xeal-leader-key)

;; Remove conflicting leader keys
(after! evil
  (general-define-key
   :unbind t
   :keymaps '(normal visual motion)
   "SPC"
   ","))

(req-package which-key :force t
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
