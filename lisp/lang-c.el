;;; lang-c.el --- C support -*- lexical-binding: t -*-

;;; Commentary:
;; Damn.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package)
  (require 'base-keybinds))

;;;
;; Functions

;; (defun +c-astyle-gnu-region (pmin pmax)
;;   "Formats the selected region using the GNU C formatting style."
;;   (interactive "r")
;;   (shell-command-on-region pmin pmax
;;                            "astyle -A7 -s2 -xB -xD"
;;                            (current-buffer) t
;;                            (get-buffer-create "*Astyle Errors*")
;;                            t))

;;;
;; Packages

(req-package clang-format
  :commands (clang-format-region clang-format-buffer)
  :preface
  (defun +clang-format-buffer-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (interactive)
    (require 'f)
    (when (f-exists? (expand-file-name ".clang-format" (projectile-project-root)))
      (clang-format-buffer)))
  :general
  (:keymaps 'c-mode-map
            :states '(normal operator)
            :prefix xeal-localleader-key
            "=" #'+clang-format-buffer-smart)
  (:keymaps 'c-mode-map
            :states '(visual)
            :prefix xeal-localleader-key
            "=" #'clang-format-region))

(req-package irony
  :hook
  (c-mode     . irony-mode)
  (c++-mode   . irony-mode)
  (irony-mode . irony-cdb-autosetup-compile-options))

(req-package company-irony
  :requires company
  :commands company-irony
  :init
  (set-company-backends 'c-mode 'company-irony))

(req-package flycheck-irony
  :requires flycheck
  :hook
  (irony-mode . flycheck-mode)
  (flycheck-mode . flycheck-irony-setup))

(req-package cc-mode)

(provide 'lang-c)
;;; lang-c.el ends here
