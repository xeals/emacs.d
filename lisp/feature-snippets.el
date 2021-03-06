;;; feature-snippets.el --- Snippet insertion -*- lexical-binding: t -*-

;;; Commentary:
;; Making life easier.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(use-package yasnippet
  :defer 2
  :mode
  ("\\.snippet$" . snippet-mode)
  ("\\.yasnippet$" . snippet-mode)
  :hook (after-init . yas-global-mode)
  ;; :general ; bound in bindings.el
  :init
  (setq yas-verbosity 3
        yas-also-indent-empty-lines t
        yas-triggers-in-field t
        yas-snippet-dirs (list (concat (expand-file-name user-emacs-directory) "snippets/")))
  :config
  (push #'+ivy-yas-prompt yas-prompt-functions)
  (after! evil
    (add-hook 'yas/before-expand-snippet-hook #'evil-insert-state)))

(use-package auto-yasnippet
  :commands (aya-create aya-expand aya-open-line aya-persist snippet)
  ;; :general ; bound in bindings.el
  :init
  (setq aya-persist-snipets-dir (x/data "auto-snippets/")))

(use-package ivy-yasnippet)

;;;
;; Autoloads

;;;###autoload
(defun +ivy-yas-prompt (prompt choices &optional display-fn)
  (yas-completing-prompt prompt choices display-fn #'ivy-completing-read))

(provide 'feature-snippets)
;;; feature-snippets.el ends here
