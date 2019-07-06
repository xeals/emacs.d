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
 fill-column 80                         ; 80 column not so master race
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

(maybe-push-exec-path (or (getenv "XDG_BIN_HOME") (expand-file-name "~/.local/bin")))

;; From https://github.com/emacscollective/no-littering
(after! x-win
  (let ((session-dir (x/cache "emacs-session/")))
    (progn
      (make-directory session-dir t)
      (defun emacs-session-filename (session-id)
        "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use `no-littering'
directories."
        (expand-file-name session-id session-dir)))))

;;;
;; Builtins

;; Revert buffers on external change
(use-feature autorevert
  :defer 2
  :init
  (setq auto-revert-verbose nil
        global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode 1))

;; Documentation lines
(use-feature eldoc)

;; Additional help stuff
;; straight.el loads from emacsmirror!
(use-package help-fns+)

;; Keep track of recently opened files
(use-feature recentf
  :defer 1
  :hook (find-file . (lambda () (unless recentf-mode (recentf-mode) (recentf-track-opened-file))))
  :init
  (setq recentf-save-file (x/cache "recentf")
        recentf-auto-cleanup 'never
        recentf-filename-handlers '(abbreviate-file-name)
        recentf-max-saved-items 100
        recentf-auto-save-timer (run-with-idle-timer 600 t #'recentf-save-list)
        recentf-exclude `("/tmp/"             ; temp files
                          "^/\\.git/.+?"      ; git contents
                          "COMMIT_EDITMSG$"
                          "recentf$"
                          "/usr/share/emacs/" ; emacs source
                          ;; `abbreviate-file-name' because that's how it's transformed
                          ;; otherwise use `expand-file-name'
                          ,(abbreviate-file-name xeal-cache-dir)
                          ,(abbreviate-file-name user-emacs-directory)
                          ,(abbreviate-file-name xeal-packages-dir)))
  :config
  (quiet! (recentf-mode 1)))

;; Persistent minibuffer history
(use-feature savehist
  :defer 1
  :init
  (setq savehist-file (x/cache "savehist")
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
(use-feature saveplace
  :demand t
  :init
  (setq save-place-file (x/cache "saveplace"))
  :config
  (save-place-mode t))

;; Code folding
(use-feature hideshow
  :hook (prog-mode . hs-minor-mode))

;;;
;; Packages

;; Handles editor-agnostic indentation settings
(use-package editorconfig
  :mode ("\\.?editorconfig$" . editorconfig-conf-mode)
  :demand t
  :init
  (autoload 'editorconfig-conf-mode "editorconfig-conf-mode" nil t)
  :config
  (editorconfig-mode 1))

;; Auto-close delimiters as I type
(use-package smartparens
  :defer 1
  :general
  (:keymaps 'smartparens-mode-map
   :states '(normal insert)
   "M-h" #'sp-backward-sexp
   "M-H" #'sp-forward-barf-sexp
   "M-k" #'sp-backward-up-sexp
   "M-j" #'sp-down-sexp
   "M-l" #'sp-forward-sexp
   "M-L" #'sp-forward-slurp-sexp)
  :init
  (setq sp-highlight-pair-overlay nil
        sp-show-pair-delay 0)
  :config
  (sp-local-pair '(xml-mode nxml-mode) "<!--" "-->"
                 :post-handlers '(("| " "SPC")))

  ;; ;; Auto-close more conservatively
  ;; (let ((unless-list '(sp-point-before-word-p
  ;;                      sp-point-after-word-p
  ;;                      sp-point-before-same-p)))
  ;;   (sp-pair "'"  nil :unless unless-list)
  ;;   (sp-pair "\"" nil :unless unless-list))
  ;; (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
  ;;          :unless '(sp-point-before-word-p sp-point-before-same-p))
  ;; (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
  ;;          :unless '(sp-point-before-word-p sp-point-before-same-p))
  ;; (sp-pair "[" nil :post-handlers '(("| " " "))
  ;;          :unless '(sp-point-before-word-p sp-point-before-same-p))

  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package writeroom-mode
  :commands writeroom-mode
  :init
  (setq writeroom-mode-line nil
        writeroom-width 100))

(use-package focus
  :commands focus-mode
  :hook (writeroom-mode . focus-mode)
  :init
  (setq focus-dimness -50))

(use-package anzu
  :hook (after-init . global-anzu-mode))

(use-package undo-tree
  :hook (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist (list (cons "." (x/data "undo-tree-hist/"))))
  :config
  ;; Hacky method to get the tree to be a bit thinner.
  ;; Code borrowed from `set-popup-buffer'.
  (cl-pushnew `(,undo-tree-visualizer-buffer-name
                (display-buffer-reuse-window
                 display-buffer-in-side-window)
                (reusable-frames . visible)
                (side            . right)
                (window-height   . 0.3))
              display-buffer-alist :test 'equal)

  (defun undo-tree-split-side-by-side (f &rest args)
    "Split undo tree side-by-side instead of above-below."
    (let ((split-height-threshold nil)
          (split-width-threshold 0)
          (split-width))
      (apply f args)))
  (advice-add #'undo-tree-visualize :around #'undo-tree-split-side-by-side))

(use-package neotree
  :general
  (:keymaps 'neotree-mode-map
   :states 'normal
   "RET" #'neotree-enter
   "TAB" #'neotree-enter
   "-" #'neotree-select-up-node
   "A" #'neotree-stretch-toggle
   "g" #'neotree-refresh
   "H" #'neotree-hidden-file-toggle
   "i" #'neotree-quick-look
   "n" #'neotree-next-line
   "p" #'neotree-previous-line
   "q" #'neotree-hide)
  :preface
  (defun +neotree/open-at-projectile ()
    "Open NeoTree at the projectile project root, if available."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-name)))
      (neotree-toggle)
      (when (and project-dir
                 (neo-global--window-exists-p))
        (neotree-dir project-dir)
        (neotree-find file-name))))
  :init
  (setq neo-smart-open nil
        neo-autorefresh nil
        neo-theme (if (display-graphic-p) 'icons 'arrow)
        projectile-switch-project-action #'neotree-projectile-action))

(use-package aggressive-indent
  :hook (after-init . global-aggressive-indent-mode))

(provide 'base-editor)
;;; base-editor.el ends here
