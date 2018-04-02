;;; base-vars.el --- Base variables -*- lexical-binding: t -*-

;;; Commentary:
;; Customisation through variables.

;;; Code:

;;;
;; XDG

(defvar xdg-cache-home
  (or (getenv "XDG_CACHE_HOME")
      (expand-file-name "~/.cache"))
  "The XDG cache base directory.")

(defvar xdg-data-home
  (or (getenv "XDG_DATA_HOME")
      (expand-file-name "~/.local/share"))
  "The XDG data base directory.")

;;;
;; Base

(defvar xeal-debug-mode nil
  "Output debugging information if non-nil.")

(defvar xeal-site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory)
  "Directory for shared files.")

(defvar xeal-cache-dir
  (concat xdg-cache-home "/emacs/")
  "Directory for volatile storage.")

(defvar xeal-data-dir
  (concat xdg-data-home "/emacs/")
  "Directory for non-volatile storage.")

(defvar xeal-packages-dir
  (concat xeal-data-dir "packages/")
  "Directory for package.el installation.")

;;;
;; Other file structures

(defvar xeal-sync-dir
  (expand-file-name "~/Sync")
  "Directory for Syncthing, containing most other stuff.")

(defvar xeal-uni-dir
  (concat xeal-sync-dir "/uni")
  "Directory containing university files.")

(defvar xeal-org-dir
  (concat xeal-sync-dir "/org"))

(defvar xeal-rp-dir
  (concat xeal-org-dir "/roleplaying")
  "Directory containing roleplaying files.")

(defvar xeal-wb-dir
  (concat xeal-org-dir "/worldbuilding")
  "Directory containing worldbuilding files.")

(defvar gtd/main-file (expand-file-name "gtd/gtd.org" xeal-sync-dir))
(defvar gtd/inbox-file (expand-file-name "gtd/inbox.org" xeal-sync-dir))
(defvar gtd/someday-file (expand-file-name "gtd/someday.org" xeal-sync-dir))

;;;
;; UI

(defvar xeal-theme 'pencil-dark
  "Colour theme to use.")

(defvar xeal-font "IBM Plex Mono"
  "Default typeface to use.")

(defvar xeal-variable-pitch-font "IBM Plex Sans"
  "Default variable pitch typeface to use.")

(defvar xeal-font-height 105
  "Default font height to use.")

(provide 'base-vars)
;;; base-vars.el ends here
