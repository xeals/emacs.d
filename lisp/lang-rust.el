;;; lang-rust.el --- Rust language support -*- lexical-binding: t -*-

;;; Commentary:
;; Corrosive.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(req-package rust-mode
  :mode "\\.rs$"
  ;; :hook
  ;; (rust-mode . eglot-ensure)
  :general
  (:keymaps 'rust-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "'" #'asf-rustdoc-edit)
  :init
  (autoload 'asf-rustdoc-edit "rustdoc-edit.el")
  (set-doc-fn 'rust-mode #'eglot-help-at-point))

(req-package racer
  ;; :disabled t
  :hook
  (rust-mode . racer-mode)
  (racer-mode . eldoc-mode)
  :preface
  (defun +racer-describe ()
    "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
    (interactive)
    (let ((window (racer-describe)))
      (when help-window-select
        (select-window window)
        (help-mode))))
  :init
  (set-doc-fn 'racer-mode #'+racer-describe))

(req-package flycheck-rust
  ;; :disabled t
  :requires flycheck
  :hook
  (flycheck-mode . flycheck-rust-setup)
  (rust-mode . flycheck-mode))

(req-package cargo
  :hook
  (rust-mode . cargo-minor-mode)
  :preface
  (defun +cargo-open-toml ()
    "Opens the project's Cargo.toml file."
    (interactive)
    (find-file (expand-file-name "Cargo.toml" (projectile-project-root))))
  :general
  (:keymaps 'cargo-minor-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "=" #'cargo-process-fmt
   "b" #'cargo-process-bench
   "c" #'cargo-process-build
   "d" #'cargo-process-doc
   "D" #'cargo-process-doc-open
   "k" #'cargo-process-check
   "m" #'(+cargo-open-toml :wk "cargo-open-toml")
   "p" #'cargo-process-clippy
   "r" #'cargo-process-run
   "R" #'cargo-process-run-example
   "s" #'cargo-process-search
   "t" #'cargo-process-current-file-tests
   "T" #'cargo-process-current-test)
  (:keymaps 'cargo-process-mode-map
   "q" #'quit-window
   ;; TODO replace with emacs-native function
   "g" #'evil-goto-first-line)
  :init
  (setq cargo-process--command-fmt    "fmt"
        cargo-process--command-check  "+nightly check"
        cargo-process--command-clippy "clippy"))

(provide 'lang-rust)
;;; lang-rust.el ends here
