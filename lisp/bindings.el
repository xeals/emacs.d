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

;; (defun increment-number-at-point ()
;;   (interactive)
;;   (skip-chars-backward "0-9")
;;   (or (looking-at "[0-9]+")
;;       (error "no number at point"))
;;   (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

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
  "'"   '(term                  :wk "terminal")
  ";"   '(eval-expression       :wk "eval expression")
  "/"   '(counsel-projectile-rg :wk "rg")
  "TAB" '(alternate-buffer      :wk "last buffer")
  "m"   `(,(general-simulate-key "," :state 'normal) :wk "major mode")
  "u"   '(universal-argument    :wk "universal argument")

  "a" '(:ignore t :wk "agenda")
  "aa" '(org-agenda-list :wk "agenda")
  "ac" '(org-capture     :wk "capture")
  "as" '(org-search-view :wk "search")
  "at" '(org-todo-list   :wk "todo")
  "aT" `(,(λ! (org-call-with-arg 'org-todo-list '(4))) :wk "todo tags")
  "ao" '(org-agenda      :wk "menu")

  "b" '(:ignore t :wk "buffer")
  "bb" '(switch-to-buffer       :wk "list buffers")
  "bd" '(+kill-this-buffer      :wk "kill buffer")
  "bD" `(kill-other-buffers     :wk "kill other buffers")
  "bm" '(switch-messages-buffer :wk "messages buffer")
  "bn" '(evil-buffer-new        :wk "new empty buffer")
  "bs" '(switch-scratch-buffer  :wk "scratch buffer")

  "B" '(:ignore t :wk "bookmarks")
  "BD" '(bookmark-delete     :wk "delete")
  "Bj" '(bookmark-jump       :wk "jump")
  "Bl" '(bookmark-bmenu-list :wk "list")
  "Br" '(bookmark-rename     :wk "rename")
  "Bs" '(bookmark-set        :wk "set")

  "c" '(:ignore t :wk "code")
  "c." '(editorconfig-apply                :wk "apply editorconfig")
  "cc" `(,(λ! (message "not available yet!")) :wk "compile")
  "cl" '(evilnc-comment-or-uncomment-lines :wk "comment lines")
  "cy" '(evilnc-copy-and-comment-lines     :wk "yank and comment lines")
  "cx" '(flycheck-list-errors              :wk "errors")

  "f" '(:ignore t :wk "file")
  "fb" '(bookmark-jump                  :wk "bookmarks")
  "fc" '(copy-file                      :wk "copy")
  "fD" '(+delete-current-buffer-file    :wk "delete current")
  "fe" '(xeal/dotfile-hydra/body        :wk "configs")
  "ff" '(counsel-find-file              :wk "find file")
  "fg" '(counsel-rg                     :wk "rg")
  "fR" '(+rename-current-buffer-file    :wk "rename current")
  "fr" '(counsel-recentf                :wk "recent")
  "fy" '(+show-and-copy-buffer-filename :wk "show and copy filename")

  "g" '(:ignore t :wk "git")
  "gs" '(magit-status  :wk "status")
  "gb" '(magit-blame   :wk "blame")
  "gl" '(magit-log-all :wk "log")

  "h" '(:ignore t :wk "help")
  "hb" '(describe-bindings :wk "bindings")
  "hc" '(describe-char     :wk "character")
  "hf" '(describe-function :wk "functions")
  "hF" '(describe-face     :wk "faces")
  "hk" '(describe-key      :wk "key")
  "hK" '(describe-keymap   :wk "keymap")
  "hm" '(describe-mode     :wk "mode")
  "hp" '(describe-package  :wk "packages")
  "hv" '(describe-variable :wk "variables")
  "hw" #'woman

  "i" '(:ignore t :wk "insert")
  "in" #'rectangle-number-lines
  "is" #'yas-insert-snippet

  "iS" '(:ignore t :wk "snippets")
  "iSn" #'yas-new-snippet
  "iSi" #'yas-insert-snippet
  "iSv" #'yas-visit-snippet-file

  "o" '(:ignore t :wk "open")
  "od" '(xeal/open-docs :wk "documentation")
  "oi" `(,(λ! (+gtd/open-file "inbox")) :wk "inbox")
  "ol" `(xeal/open-ledger :wk "ledger")
  "on" '(xeal/open-notes :wk "notes")
  "os" `(,(λ! (+gtd/open-file "someday")) :wk "someday")
  "ot" `(,(λ! (+gtd/open-file "gtd")) :wk "todo")
  "or" `(,(λ! (counsel-find-file xeal-rp-dir)) :wk "roleplaying")
  "ow" `(,(λ! (counsel-find-file xeal-wb-dir)) :wk "worldbuilding")

  "ou" '(:ignore t :wk "university")
  "oua" `(,(λ! (xeal/open-uni "assignments")) :wk "assignments")
  "oun" '(+org/open-notes/body :wk "notes")
  "oul" `(,(λ! (xeal/open-uni "labs")) :wk "labs")
  "out" `(,(λ! (xeal/open-uni "tutoring")) :wk "tutoring")

  "p" '(:ignore t :wk "project")
  "p SPC" '(counsel-projectile-find-file :wk "find file in project")
  "p!"   '(projectile-run-shell-command-in-root :wk "run cmd in root")
  "pb"   '(counsel-projectile-bookmark :wk "project bookmarks")
  "pf"   '(counsel-projectile-find-file :wk "find file in project")
  "pI"   '(projectile-invalidate-cache :wk "invalidate cache")
  "pp"   '(counsel-projectile-switch-project :wk "switch project")
  "pr"   '(projectile-recentf :wk "recent projects")
  "pt"   '(+ivy/tasks :wk "find project tasks")

  "q" '(:ignore t :wk "quit")
  "qq" '(evil-save-and-quit :wk "quit")
  "qr" '(restart-emacs :wk "restart")

  "t" '(:ignore t :wk "toggles/theme")
  "tl" #'+line-numbers-toggle
  "tL" #'global-hl-line-mode
  "tr" #'rainbow-mode
  "ts" #'flyspell-mode
  "tt" #'counsel-load-theme
  "tv" #'visual-line-mode
  "tW" #'writegood-mode

  "v" '(:ignore t :wk "window")
  "vv" '(split-window-right           :wk "split vertical")
  "vV" '(split-window-right-and-focus :wk "split vertical and focus")
  "vs" '(split-window-below           :wk "split horizontal")
  "vS" '(split-window-below-and-focus :wk "split horizontal")
  "v=" '(balance-windows-area         :wk "balance windows area")
  ;; "c" '(centered-buffer-mode         :wk "center buffer")
  "vd" '(delete-window                :wk "delete window")
  "vh" '(evil-window-left             :wk "select left")
  "vl" '(evil-window-right            :wk "select right")
  "vk" '(evil-window-up               :wk "select up")
  "vj" '(evil-window-down             :wk "select down")
  "vH" '(evil-move-window-far-left    :wk "move left")
  "vL" '(evil-move-window-far-right   :wk "move right")
  "vK" '(evil-move-window-very-top    :wk "move up")
  "vJ" '(evil-move-window-very-bottom :wk "move down")
  "vu" '(winner-undo                  :wk "Winner undo")
  "vU" '(winner-redo                  :wk "Winner redo")
  "vv" '(+evil/nav-hydra/body         :wk "hydra")
  "vw" '(writeroom-mode               :wk "writeroom"))

(after! evil
  (general-define-key
   :unbind t
   :keymaps 'normal
   "n" "N" "S")

  ;; Normal state
  (general-define-key
   :keymaps 'normal
   "-" #'dired-jump
   "S" #'save-buffer

   "n" (λ! (evil-ex-search-next) (evil-scroll-line-to-center nil))
   "N" (λ! (evil-ex-search-previous) (evil-scroll-line-to-center nil))

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
