;;; tool-quick-find.el --- Easy discovery functions -*- lexical-binding: t -*-

;;; Commentary:
;; Help me help myself.

;;; Code:

(eval-when-compile
  (require 'base-vars))

;;;
;; Functions

(defun xeal/find-dotfiles ()
  "Open the lisp dotfile directory."
  (interactive)
  (counsel-find-file (concat user-emacs-directory "lisp/")))

(defun xeal/counsel-open-config (folder)
  (interactive)
  (counsel-find-file (concat xdg-config-home "/" folder)))

;; University

(defun xeal/open-uni (dir)
  "Opens the university subfolder specified by DIR.
The base university folder is given by `xeal/uni-dir'."
  (interactive)
  (counsel-find-file (concat xeal-uni-dir "/" dir "/")))

(defun xeal/open-docs ()
  "Opens the documentation file."
  (interactive)
  (find-file (concat xeal-sync-dir "/org/docs.org")))

(defun xeal/open-notes ()
  "Opens the note file."
  (interactive)
  (find-file (concat xeal-sync-dir "/org/notes.org")))

(defun +gtd/open-file (filename)
  "Opens the Getting Things Done file."
  (interactive)
  (find-file (concat xeal-sync-dir "/gtd/" filename ".org")))

(provide 'tool-quick-find)
;;; tool-quick-find.el ends here
