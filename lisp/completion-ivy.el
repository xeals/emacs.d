;;; completion-ivy.el --- Completion system -*- lexical-binding: t -*-

;;; Commentary:
;; Completing all your things.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package))

;;;
;; Packages

(req-package ivy
  :hook (after-init . ivy-mode)
  :general
  (:keymaps 'ivy-minibuffer-map
            [escape] #'keyboard-escape-quit
            "ESC"    #'keyboard-escape-quit
            "C-w"    #'ivy-backward-kill-word
            "C-j"    #'ivy-next-line
            "C-k"    #'ivy-previous-line)
  (:keymaps 'ivy-mode-map
            [remap switch-to-buffer] #'ivy-switch-buffer
            [remap imenu-anywhere]   #'ivy-imenu-anywhere)
  :init
  (setq ivy-height 15
        ivy-wrap t
        ivy-initial-inputs-alist nil    ; fuck that caret
        ivy-format-function #'ivy-format-function-line
        ivy-format-function #'ivy-format-function-line
        ivy-magic-slash-non-match-action #'ivy-magic-slash-non-match-cd-selected
        ivy-use-selectable-prompt t
        ivy-use-virtual-buffers nil
        ivy-ignore-buffers '("\\` "
                             "\\*scratch\\*"
                             "\\*GNU Emacs\\*"
                             "\\*Help\\*"
                             "\\*Completions\\*"
                             "\\*Messages\\*"
                             "\\*Compile-Log\\*"
                             "\\*package-build-checkout\\*"))
  :config
  (after! magit (setq magit-completing-read-function #'ivy-completing-read))

  (nconc ivy-sort-functions-alist
         '((persp-kill-buffer   . nil)
           (persp-remove-buffer . nil)
           (persp-add-buffer    . nil)
           (persp-switch        . nil)
           (persp-window-switch . nil)
           (persp-frame-switch  . nil))))

(req-package ivy-posframe
  :after ivy
  :commands (ivy-posframe-display-at-frame-center ivy-posframe-enable)
  :init
  (setq ivy-display-function #'ivy-posframe-display-at-frame-center
        ivy-posframe-width (floor (* (frame-width) 0.7)))
  :config
  (ivy-posframe-enable))

(req-package all-the-icons-ivy
  :after ivy
  :hook (ivy-mode . all-the-icons-ivy-setup))

(req-package swiper
  :commands (swiper swiper-multi swiper-all))

(req-package smex
  :commands (smex smex-major-mode-commands)
  :init
  (setq smex-save-file (x/cache "smex"))
  :config
  (smex-initialize))

(req-package counsel
  :demand t
  :after ivy
  :general
  (:keymaps 'ivy-mode-map
            [remap apropos]                   #'counsel-apropos
            [remap bookmark-jump]             #'counsel-bookmark
            [remap describe-face]             #'counsel-describe-face
            [remap describe-face]             #'counsel-describe-face
            [remap describe-function]         #'counsel-describe-function
            [remap describe-variable]         #'counsel-describe-variable
            [remap eshell-list-history]       #'counsel-esh-history
            [remap execute-extended-command]  #'counsel-M-x
            [remap find-file]                 #'counsel-find-file
            [remap imenu]                     #'counsel-imenu
            [remap recentf-open-files]        #'counsel-recentf)
  :init
  (setq counsel-find-file-ignore-regexp
       "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
       counsel-grep-base-command
       "rg -i -M 120 --no-heading --line-number --color never '%s' %s"
       counsel-bookmark-avoid-dired t))

(req-package counsel-projectile
  :demand t
  :after (counsel projectile)
  :general
  (:keymaps 'ivy-mode-map
            [remap projectile-find-dir]         #'counsel-projectile-find-dir
            [remap projectile-find-file]        #'counsel-projectile-find-file
            [remap projectile-switch-project]   #'counsel-projectile-switch-project
            [remap projectile-switch-to-buffer] #'counsel-projectile-switch-to-buffer))

(provide 'completion-ivy)
;;; completion-ivy.el ends here
