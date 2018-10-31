;;; base.el --- Base configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Setting sane defaults.

;;; Code:

(require 'base-vars)

(defvar xeal-startup-time nil
  "The time it took, in seconds, for Emacs to initialise.")

;;;
;; Settings

;; Unicode please.
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq-default buffer-file-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

(setq-default
 ;; mode-line-default-help-echo nil        ; disable mode-line mouseovers
 ;; History and backup
 auto-save-default nil                  ; no autosaving
 create-lockfiles nil                   ; no lockfiles
 make-backup-files nil                  ; no backups
 ;; X and clipboard
 select-enable-clipboard t              ; merge with system clipboard
 select-enable-primary t                ; merge some more
 x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)
 x-gtk-use-system-tooltips t            ; use GTK tooltips
 ;; Files
 abbrev-file-name            (concat xeal-data-dir "abbrev.el")
 auto-save-list-file-name    (concat xeal-cache-dir "autosave")
 backup-directory-alist      (list (cons "." (concat xeal-cache-dir "backup/")))
 bookmark-default-file       (concat xeal-data-dir "bookmarks")
 eshell-directory-name       (concat xeal-data-dir "eshell/")
 eshell-history-file-name    (concat xeal-data-dir "eshell-history")
 pcache-directory            (concat xeal-cache-dir "pcache/")
 server-auth-dir             (concat xeal-cache-dir "server/")
 shared-game-score-directory (concat xeal-data-dir "shared-game-score/")
 tramp-auto-save-directory   (concat xeal-cache-dir "tramp/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name (concat xeal-cache-dir "tramp-persistency.el")
 url-cache-directory         (concat xeal-cache-dir "url/")
 url-configuration-directory (concat xeal-data-dir "url/"))

(fset 'yes-or-no-p 'y-or-n-p)

;; Move custom out of init.el
(setq-default custom-file (concat xeal-data-dir "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file nil t))

;;;
;; Initialise
(eval-and-compile
  (require 'cl-lib)
  (require 'base-package)

  (eval-when-compile
    (+packages-initialise))

  (setq load-path (eval-when-compile load-path)
        custom-theme-load-path (eval-when-compile custom-theme-load-path))

  (require 'base-lib))

(require 'server)
(unless (server-running-p)
  (server-start))

;;;
;; Bootstrap

(unless noninteractive
  (require 'base-keybinds)
  (require 'base-popups)
  (require 'base-projects)
  (require 'base-modeline)
  (require 'base-editor)
  (require 'base-ui))

(provide 'base)
;;; base.el ends here
