;;; bindings.el --- Key input commands -*- lexical-binding: t -*-

;;; Commentary:
;; Governor general reporting.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-vars))

;;;
;; Functions

(defmacro λ! (&rest body)
  "Wraps BODY in an interactive lambda function."
  (declare (doc-string 1))
  `(lambda () (interactive) ,@body))

(defun +counsel-grep-or-swiper-no-visual ()
  "Toggles `visual-line-mode' before and after running
`counsel-grep-or-swiper' to attempt to get around the delay that
`visual-line-mode' causes to it. Currently only re-enables if the
search is followed (not cancelled)."
  (interactive)
  (let ((was-visual visual-line-mode))
    (when was-visual (visual-line-mode -1))
    (counsel-grep-or-swiper)
    (when was-visual (visual-line-mode +1))))

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "no number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

(defun change-number-at-point (change)
  (let ((number (number-at-point))
        (point (point)))
    (when number
      (forward-word)
      (search-backward (number-to-string number))
      (replace-match (number-to-string (funcall change number)))
      (goto-char point))))

(defun increment-number-at-point ()
  (interactive)
  (change-number-at-point '1+))

(defun decrement-number-at-point ()
  (interactive)
  (change-number-at-point '1-))

;;;
;; Global

(general-define-key
 :keymaps 'global
 [escape] #'evil-escape

 ;; Search
 "C-s" #'+counsel-grep-or-swiper-no-visual

 ;; Text scaling
 "M-+" #'text-scale-increase
 "M--" #'text-scale-decrease)

;; Leader

(general-leader
  "" nil
  "SPC" '(counsel-M-x           :wk "M-x")
  "!"   '(shell-command         :wk "shell command")
  "'"   '(eshell                :wk "terminal")
  ";"   '(eval-expression       :wk "eval expression")
  "/"   '(counsel-projectile-rg :wk "rg")
  "TAB" '(alternate-buffer      :wk "last buffer")
  "u"   '(universal-argument    :wk "universal argument"))

(general-leader
  :infix "a"
  "" '(:ignore t :wk "agenda")
  "a" '(org-agenda-list :wk "agenda")
  "c" '(org-capture     :wk "capture")
  "s" '(org-search-view :wk "search")
  "t" '(org-todo-list   :wk "todo")
  "T" `(,(λ! (org-call-with-arg 'org-todo-list '(4))) :wk "todo tags")
  "o" '(org-agenda      :wk "menu"))

(general-leader
  :infix "b"
  "" '(:ignore t :wk "buffer")
  "b" '(switch-to-buffer       :wk "list buffers")
  "d" '(+kill-this-buffer      :wk "kill buffer")
  "D" `(kill-other-buffers     :wk "kill other buffers")
  "m" '(switch-messages-buffer :wk "messages buffer")
  "n" '(evil-buffer-new        :wk "new empty buffer")
  "s" '(switch-scratch-buffer  :wk "scratch buffer"))

(general-leader
  :infix "k"
  "" '(:ignore t :wk "bookmarks")
  "D" '(bookmark-delete     :wk "delete")
  "j" '(bookmark-jump       :wk "jump")
  "l" '(bookmark-bmenu-list :wk "list")
  "r" '(bookmark-rename     :wk "rename")
  "s" '(bookmark-set        :wk "set"))

(general-leader
  :infix "c"
  "" '(:ignore t :wk "code")
  "." '(editorconfig-apply                :wk "apply editorconfig")
  "c" `(,(λ! (message "not available yet!")) :wk "compile")
  "l" '(evilnc-comment-or-uncomment-lines :wk "comment lines")
  "y" '(evilnc-copy-and-comment-lines     :wk "yank and comment lines")
  "x" '(flycheck-list-errors              :wk "errors"))

(general-leader
  :infix "f"
  "" '(:ignore t :wk "file")
  "b" '(bookmark-jump                  :wk "bookmarks")
  "c" '(copy-file                      :wk "copy")
  "D" '(+delete-current-buffer-file    :wk "delete current")
  "f" '(counsel-find-file              :wk "find file")
  "e" '(xeal/find-dotfiles             :wk "open dotfiles folder")
  "g" '(counsel-rg                     :wk "rg")
  "R" '(+rename-current-buffer-file    :wk "rename current")
  "r" '(counsel-recentf                :wk "recent")
  "y" '(+show-and-copy-buffer-filename :wk "show and copy filename"))

(general-leader
  :infix "g"
  "" '(:ignore t :wk "git")
  "s" '(magit-status  :wk "status")
  "b" '(magit-blame   :wk "blame")
  "l" '(magit-log-all :wk "log"))

(general-leader
  :infix "h"
  "" '(:ignore t :wk "help")
  "b" '(describe-bindings :wk "bindings")
  "c" '(describe-char     :wk "character")
  "f" '(describe-function :wk "functions")
  "F" '(describe-face     :wk "faces")
  "k" '(describe-key      :wk "key")
  "K" '(describe-keymap   :wk "keymap")
  "m" '(describe-mode     :wk "mode")
  "p" '(describe-package  :wk "packages")
  "v" '(describe-variable :wk "variables"))

(general-leader
  :infix "i"
  "" '(:ignore t :wk "insert")
  "n" #'rectangle-number-lines)

(general-leader
  :infix "o"
  "" '(:ignore t :wk "open")
  "d" '(xeal/open-docs :wk "documentation")
  "i" `(,(λ! (+gtd/open-file "inbox")) :wk "inbox")
  "n" '(xeal/open-notes :wk "notes")
  "s" `(,(λ! (+gtd/open-file "someday")) :wk "someday")
  "t" `(,(λ! (+gtd/open-file "gtd")) :wk "todo")
  "r" `(,(λ! (counsel-find-file xeal-rp-dir)) :wk "roleplaying")
  "w" `(,(λ! (counsel-find-file xeal-wb-dir)) :wk "worldbuilding"))

(general-leader
  :infix "ou"
  "" '(:ignore t :wk "university")
  "a" `(,(λ! (xeal/open-uni "assignments")) :wk "assignments")
  "n" '(+org/open-notes/body :wk "notes")
  "l" `(,(λ! (xeal/open-uni "labs")) :wk "labs")
  "t" `(,(λ! (xeal/open-uni "tutoring")) :wk "tutoring"))

(general-leader
  :infix "p"
  "" '(:ignore t :wk "project")
  "!"   '(projectile-run-shell-command-in-root :wk "run cmd in root")
  "SPC" '(counsel-projectile-find-file :wk "find file in project")
  "b"   '(counsel-projectile-bookmark :wk "project bookmarks")
  "f"   '(counsel-projectile-find-file :wk "find file in project")
  "I"   '(projectile-invalidate-cache :wk "invalidate cache")
  "p"   '(counsel-projectile-switch-project :wk "switch project")
  "r"   '(projectile-recentf :wk "recent projects")
  "t"   '(+ivy/tasks :wk "find project tasks"))

(general-leader
  :infix "q"
  "" '(:ignore t :wk "quit")
  "q" '(evil-save-and-quit :wk "quit")
  "r" '(restart-emacs :wk "restart"))

(general-leader
  :infix "t"
  "" '(:ignore t :wk "toggles/theme")
  "l" #'+line-numbers-toggle
  "L" #'global-hl-line-mode
  "r" #'rainbow-mode
  "s" #'flyspell-mode
  "t" #'counsel-load-theme
  "v" #'visual-line-mode
  "W" #'writegood-mode)

(general-leader
  :infix "w"
  "" '(:ignore t :wk "window")
  "v" '(split-window-right           :wk "split vertical")
  "V" '(split-window-right-and-focus :wk "split vertical and focus")
  "s" '(split-window-below           :wk "split horizontal")
  "S" '(split-window-below-and-focus :wk "split horizontal")
  "=" '(balance-windows-area         :wk "balance windows area")
  ;; "c" '(centered-buffer-mode         :wk "center buffer")
  "d" '(delete-window                :wk "delete window")
  "h" '(evil-window-left             :wk "select left")
  "l" '(evil-window-right            :wk "select right")
  "k" '(evil-window-up               :wk "select up")
  "j" '(evil-window-down             :wk "select down")
  "H" '(evil-move-window-far-left    :wk "move left")
  "L" '(evil-move-window-far-right   :wk "move right")
  "K" '(evil-move-window-very-top    :wk "move up")
  "J" '(evil-move-window-very-bottom :wk "move down")
  "u" '(winner-undo                  :wk "Winner undo")
  "U" '(winner-redo                  :wk "Winner redo")
  "w" '(writeroom-mode               :wk "writeroom"))

(after! evil
  (general-define-key
   :unbind t
   :keymaps 'normal
   "S")

  ;; Normal state
  (general-define-key
   :keymaps 'normal
   "-" #'dired-jump
   "S" #'save-buffer

   ;; Numbers
   "C-a" #'increment-number-at-point
   "C-c x" #'decrement-number-at-point

   ;; Window navigation
   "C-h" #'evil-window-left
   "C-j" #'evil-window-down
   "C-k" #'evil-window-up
   "C-l" #'evil-window-right)

  (general-define-key
   :keymaps 'insert
   "C-;" #'yas-expand
   "C-/" #'aya-expand)

  (general-define-key
   :keymaps '(normal motion)
   "K"   #'documentation-at-point
   "TAB" #'indent-for-tab-command
   "C-/" #'aya-create

   "]e" #'next-error
   "[e" #'previous-error)

  (general-define-key
   :keymaps 'evil-window-map
   "z" '(maximize-window :wk "maximize window")))

(provide 'bindings)
;;; bindings.el ends here
