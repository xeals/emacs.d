;;; feature-preview.el --- Preview buffers -*- lexical-binding: t -*-

;;; Commentary:
;; WYSIWYG

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package impatient-mode
  :commands impatient-mode
  :preface
  (defun +impatient/start ()
    (httpd-start)
    (impatient-mode +1))
  :hook (mhtml-mode . +impatient/start)
  :config
  (defun +imp-markdown-filter (in)
    (let ((out (current-buffer)))
      (with-current-buffer in
        (markdown out))))

  (push (cons 'markdown-mode #'+imp-markdown-filter)
        imp-default-user-filters))

(provide 'feature-preview)
;;; feature-preview.el ends here
