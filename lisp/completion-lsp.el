;;; completion-lsp.el --- Language Server Protocol support -*- lexical-binding: t -*-

;;; Commentary:
;; Some new competition in completion.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))


;;;
;; Packages

(req-package lsp-mode
  :config
  (set-doc-fn 'lsp-mode #'lsp-describe-thing-at-point)
  ;; (set-doc-fn 'lsp-mode #'+lsp/describe-thing-at-point-popup)
  (set-popup-buffer (rx bos "*lsp-help*" eos)))

(req-package company-lsp
  :init
  (setq company-lsp-async t)
  (push 'company-lsp company-backends))

(req-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :init
  (defun +lsp/describe-thing-at-point-popup ()
    "Display the full documentation of the thing at point in a childframe."
    (interactive)
    (lsp--cur-workspace-check)
    (let* ((client (lsp--workspace-client lsp--cur-workspace))
           (contents (gethash "contents"
                              (lsp--send-request
                               (lsp--make-request
                                "textDocument/hover"
                                (lsp--text-document-position-params)))))
           (bounds (bounds-of-thing-at-point 'symbol)))
      (setq lsp-ui-doc--bounds bounds)
      (lsp-ui-doc--display (thing-at-point 'symbol t) contents)))
  (defun +lsp-ui/toggle-doc ()
    (interactive)
    (if lsp-ui-doc-enable
        (lsp-ui-doc-enable nil)
      (lsp-ui-doc-enable t)))
  (defun +lsp-ui/toggle-sideline ()
    (interactive)
    (if lsp-ui-sideline-enable
        (lsp-ui-sideline-enable nil)
      (lsp-ui-sideline-enable t)))
  (defmacro +lsp-ui/toggle (feature)
    (let ((sym (intern (concat "lsp-ui-" feature "-enable"))))
      `(if ,sym
           `(,sym nil)
         `(,sym t))))
  :general
  (:keymaps 'lsp-ui-mode-map
            :states '(normal visual operator)
            :prefix xeal-leader-key
            "tp" '(:ignore t :wk "lsp-ui")
            "tpd" '(+lsp-ui/toggle-doc :wk "lsp-ui-doc-toggle")
            "tps" '(+lsp-ui/toggle-doc :wk "lsp-ui-sideline-toggle")
            "tpp" #'lsp-ui-toggle)
  :config
  (lsp-ui-doc-enable nil)
  (req-package lsp-flycheck
    :disabled t
    :ensure f
    :after flycheck)
  (req-package lsp-ui-sideline
    :ensure f)
  (req-package lsp-ui-peek
    :disabled t
    :ensure f)
  (req-package lsp-ui-doc
    :ensure f))

(provide 'completion-lsp)
;;; completion-lsp.el ends here
