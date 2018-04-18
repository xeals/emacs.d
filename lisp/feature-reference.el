;;; feature-reference.el --- Referencing support -*- lexical-binding: t -*-

;;; Commentary:
;; BibTeX and org-ref; making academia easy.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-keybinds)
  (require 'base-vars))

;;;
;; Functions

(defvar-local bibtex-narrowed nil
  "Whether or not the current buffer is narrowed on a single entry.")

(defun +bibtex/toggle-narrow ()
  (interactive)
  (if bibtex-narrowed
      (progn
        (widen)
        (setq bibtex-narrowed nil))
    (progn
      (bibtex-narrow-to-entry)
      (setq bibtex-narrowed t))))

;;;
;; Packages

(req-package bibtex
  :general
  (:keymaps 'bibtex-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "/" #'bibtex-search-entry
            "D" #'bibtex-kill-entry
            "d" #'bibtex-kill-field
            "f" #'bibtex-make-field
            "h" #'org-ref-bibtex-hydra/body
            "n" #'bibtex-entry
            "s" #'bibtex-sort-buffer
            "u" #'bibtex-entry-update
            "w" #'+bibtex/toggle-narrow))

(req-package org-ref
  :preface
  (defun +org/load-org-ref ()
    (require 'org-ref))
  :hook (org-mode . +org/load-org-ref)
  :general
  (:keymaps 'org-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "i"
            "c" #'org-ref-helm-insert-cite-link)
  :init
  (setq reftex-default-bibliography `(,(expand-file-name "references.bib" xeal-uni-dir))))


(provide 'feature-reference)
;;; feature-reference.el ends here
