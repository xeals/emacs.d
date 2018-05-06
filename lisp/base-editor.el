;;; base-editor.el --- Editor configuration -*- lexical-binding: t -*-

;;; Commentary:
;; How and why do we work?

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package)
  (require 'base-lib)
  (require 'base-keybinds))

;;;
;; Settings

(setq-default
 save-interprogram-paste-before-kill t  ; paste before it's erased
 vc-follow-symlinks t                   ; use symlinks
 bookmark-save-flag 0                   ; save whenever I add a bookmark
 delete-by-moving-to-trash t            ; send to trash instead of deleting
 ;; Fill and wrap
 fill-column 100                        ; 80 column not so master race
 word-wrap t                            ; wrap words
 truncate-lines t                       ; please wrap words
 ;; Tabs and whitespace
 indent-tabs-mode nil                   ; never tabs
 tab-always-indent 'complete            ; complete after indenting
 tab-width 4                            ; personal preferences
 delete-trailing-lines t                ; trailing lines trigger me
 require-final-newline t                ; but one is fine
 )

(add-hook 'before-save-hook #'delete-trailing-whitespace)

(let ((xdg-bin-home (or (getenv "XDG_BIN_HOME")
                        (expand-file-name "~/.local/bin"))))
  (unless (member-ignore-case xdg-bin-home exec-path)
    (setq exec-path (append exec-path `(,xdg-bin-home)))))

;;;
;; Builtins

;; Revert buffers on external change
(req-package autorevert
  :defer 2
  :init
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

;; Documentation lines
(req-package eldoc)

;; Additional help stuff
(req-package help-fns+
  :el-get t :ensure nil)

;; Keep track of recently opened files
(req-package recentf
  :defer 1
  :hook (find-file . (lambda () (unless recentf-mode (recentf-mode) (recentf-track-opened-file))))
  :init
  (setq recentf-save-file (concat xeal-cache-dir "recentf")
        recentf-auto-cleanup 'never
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-saved-items 100
        recentf-auto-save-timer (run-with-idle-timer 600 t #'recentf-save-list)
        recentf-exclude `("/tmp/" ; temp files
                          "^/\\.git/.+?" ; git contents
                          "COMMIT_EDITMSG$"
                          "recentf$"
                          ,(expand-file-name xeal-cache-dir)
                          ,(expand-file-name user-emacs-directory)
                          ,(expand-file-name xeal-packages-dir)))
  :config
  (quiet! (recentf-mode 1)))

;; Persistent minibuffer history
(req-package savehist
  :defer 1
  :init
  (setq savehist-file (concat xeal-cache-dir "savehist")
        enable-recursive-minibuffers t
        history-length 500
        savehist-autosave-interval 60
        savehist-save-minibuffer-history t
        savehist-additional-variables '(kill-ring
                                        search-ring
                                        regexp-search-ring))
  :config
  (savehist-mode t))

;; Persistent point place
(req-package saveplace
  :demand t
  :init
  (setq save-place-file (concat xeal-cache-dir "saveplace"))
  :config
  (save-place-mode t))

;; Code folding
(req-package hideshow
  :hook (prog-mode . hs-minor-mode))

;;;
;; Packages

;; Handles editor-agnostic indentation settings
(req-package editorconfig
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :demand t
  :init
  (autoload 'editorconfig-conf-mode "editorconfig-conf-mode" nil t)
  :config
  (editorconfig-mode 1))

;; Auto-close delimiters as I type
(req-package smartparens
  :defer 1
  :init
  (setq sp-highlight-pair-overlay nil
        sp-show-pair-delay 0)
  :config
  (sp-local-pair '(xml-moe nxml-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC")))

  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))

  (require 'smartparens-config)
  (smartparens-global-mode 1))

;; Peek definition (display function source inline)
(req-package source-peek
  :el-get t :ensure nil
  :commands source-peek)

(req-package writeroom-mode
  :commands writeroom-mode
  :init
  (setq writeroom-mode-line nil
        writeroom-width 100))

;; TODO Look into undo-tree

(provide 'base-editor)
;;; base-editor.el ends here
