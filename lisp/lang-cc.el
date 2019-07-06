;;; lang-cc.el --- C support -*- lexical-binding: t -*-

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
;; Definitions

(after! flycheck
  (flycheck-define-checker cuda-nvcc
    "A CUDA checker."
    :command ("nvcc"
              "-I"
              "."
              source)
    :standard-input t
    :error-patterns
    ((warning line-start (file-name) "(" line "): warning: " (message) line-end)
     (error line-start (file-name) "(" line "): error: " (message) line-end)
     (error line-start (file-name) ":" line ":" column ": error: " (message) line-end)
     (error line-start (file-name) ":" line ":" column ": fatal error: " (message) line-end))
    :modes (cuda-mode))
  (add-to-list 'flycheck-checkers 'cuda-nvcc))

;;;
;; Packages

(use-package clang-format
  :commands (clang-format-region clang-format-buffer)
  :preface
  (defun +clang-format-maybe-format ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (when (locate-dominating-file "." ".clang-format")
      (clang-format-buffer))
    nil)
  (defun +clang-format-enable-smart ()
    "Reformat buffer if .clang-format exists in the projectile root."
    (interactive)
    (require 'f)
    (+clang-format-maybe-format))
  (defun +clang-format-enable-on-save ()
    (add-to-list 'write-file-functions #'+clang-format-maybe-format))
  :hook
  (c-mode   . +clang-format-enable-on-save)
  (c++-mode . +clang-format-enable-on-save)
  :general
  (:keymaps 'c-mode-map
   :states '(normal operator)
   :prefix xeal-localleader-key
   "=" #'+clang-format-enable-smart)
  (:keymaps 'c-mode-map
   :states '(visual)
   :prefix xeal-localleader-key
   "=" #'clang-format-region))

(use-package irony
  :hook
  (c-mode     . irony-mode)
  (c++-mode   . irony-mode)
  (irony-mode . irony-cdb-autosetup-compile-options)
  :init
  (setq irony-user-dir (x/data "irony/")))

(use-package company-irony
  :commands company-irony
  :init
  (set-company-backends 'c-mode 'company-irony))

(use-package flycheck-irony
  :hook
  (irony-mode . flycheck-mode)
  (flycheck-mode . flycheck-irony-setup))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :config
  (add-to-list 'company-c-headers-modes
               `(cuda-mode . ,(cdr (assoc 'c++-mode company-c-headers-modes))))
  (add-to-list 'company-c-headers-path-system "/usr/include/c++/8.2.1"))

(use-feature cc-mode)

(use-feature woman
  :init
  (setq woman-cache-filename (x/cache "woman.el")
        woman-use-topic-at-point t)
  :config
  (push "/opt/cuda/doc/man" woman-manpath))

(use-package cuda-mode
  :straight
  (el-patch :type git :host github :repo "emacsmirror/cuda-mode"
            :fork (:host nil :repo "https://git.owari.cc/xeals/cuda-mode.git"))
  :hook
  (cuda-mode . +clang-format-enable-on-save)
  :init
  (set-doc-fn 'cuda-mode #'woman))

(provide 'lang-cc)
;;; lang-cc.el ends here
