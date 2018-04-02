;;; lang-go.el --- Go language support -*- lexical-binding: t -*-

;;; Commentary:
;; Web servers and stuff.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Functions

;; Replaces `go--godoc'
(defun +go//godoc (query command)
  (+go//godoc-help-buf (+go//godoc-string query command)))

;; Partially replaces `go--godoc'
(defun +go//godoc-string (query command)
  (unless (string= query "")
    (shell-command-to-string (concat command " " query))))

;; Borrowed from `racer--help-buf'
(defun +go//godoc-help-buf (contents)
  (let ((buf (get-buffer-create "*godoc*"))
        ;; If the buffer already existed, we need to be able to
        ;; override `buffer-read-only'
        (inhibit-read-only t))
    (with-current-buffer buf
      (erase-buffer)
      (insert contents)
      (setq buffer-read-only t)
      (goto-char (point-min))
      (godoc-mode))
    buf))

(defun +godoc-and-godef (point)
  "Use a combination of godef and godoc to guess the documentation at POINT.

Due to a limitation in godoc, it is not possible to differentiate
between functions and methods, which may cause `godoc-at-point'
to display more documentation than desired.  Furthermore, it
doesn't work on package names or variables.

Consider using ‘godoc-gogetdoc’ instead for more accurate results.

Creates and focuses a temporary buffer."
  (condition-case nil
      (let* ((output (godef--call point))
             (file (car output))
             (name-parts (split-string (cadr output) " "))
             (first (car name-parts)))
        (if (not (godef--successful-p file))
            (message "%s" (godef--error file))
          (let ((buf (+go//godoc (format "%s %s"
                                         (file-name-directory file)
                                         (if (or (string= first "type") (string= first "const"))
                                             (cadr name-parts)
                                           (car name-parts)))
                                 godoc-and-godef-command)))
            (temp-buffer-window-show buf)
            (switch-to-buffer-other-frame buf)))))
  (file-error (message "Could not run godef binary")))

(defun +godoc-gogetdoc (point)
  "Use the gogetdoc tool to find the documentation for an identifier at POINT.

You can install gogetdoc with 'go get -u github.com/zmb3/gogetdoc'.

Creates and focuses a temporary buffer."
  (if (not (buffer-file-name (go--coverage-origin-buffer)))
      ;; TODO: gogetdoc supports unsaved files, but not introducing
      ;; new artifical files, so this limitation will stay for now.
      (error "Cannot use gogetdoc on a buffer without a file name"))
  (let ((posn (format "%s:#%d" (shell-quote-argument (file-truename buffer-file-name)) (1- (position-bytes point))))
        (out (godoc--get-buffer "<at point>")))
    (with-current-buffer (get-buffer-create "*go-gogetdoc-input*")
      (setq buffer-read-only nil)
      (erase-buffer)
      (go--insert-modified-files)
      (call-process-region (point-min) (point-max) "gogetdoc" nil out nil
                           "-modified"
                           (format "-pos=%s" posn)))
    (with-current-buffer out
      (goto-char (point-min))
      (godoc-mode)
      (temp-buffer-window-show out)
      (switch-to-buffer-other-frame out))))

(defun +godoc-at-point (point)
  "Show Go documentation for the identifier at POINT.

It uses `godoc-at-point-function' to look up the documentation."
  (interactive "d")
  (funcall godoc-at-point-function point))

(defun +gogetdoc-at-point (point)
  "Show Go documentation for the identifier at POINT.

Uses `+godoc-gogetdoc' to look up documentation."
  (interactive "d")
  (+godoc-gogetdoc point))

(defun +go/local-import-dir ()
  "Finds the import path of the current working buffer."
  (replace-regexp-in-string (concat (getenv "GOPATH") "/src/")
                            ""
                            (projectile-project-root)))

;;;
;; Packages

(req-package go-mode
  :mode "\\.go$"
  :hook
  (go-mode . flycheck-mode)
  (go-mode . +go-mode-setup)
  :preface
  (defun +go-mode-setup ()
    (add-hook 'before-save-hook #'gofmt-before-save nil t))
  :general
  (:keymaps 'go-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "d" #'godef-jump-other-window
            "h" #'+gogetdoc-at-point)
  (:keymaps 'go-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "i"
            "" '(:ignore t :wk "imports")
            "g" #'go-goto-imports
            "a" #'go-import-add
            "r" #'go-remove-unused-imports)
  (:keymaps 'go-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "g"
            "" '(:ignore t :wk "goto")
            "a" #'go-goto-arguments
            "d" #'go-goto-docstring
            "f" #'go-goto-function
            "i" #'go-goto-imports
            "n" #'go-goto-function-name
            "r" #'go-goto-return-values
            "m" #'go-goto-method-receiver)
  :init
  (setq flycheck-go-build-install-deps nil
        godoc-at-point-function #'+godoc-and-godef)
  ;; Initialise paths
  (unless (getenv "GOPATH")
    (let* ((gopath (expand-file-name "go"  xdg-data-home))
           (gobin  (expand-file-name "bin" gopath)))
      (setenv "GOPATH" gopath)
      (setenv "GOBIN"  gobin)
      (unless (member-ignore-case gobin exec-path)
        (setq exec-path (append exec-path `(,gobin))))))
  :config
  (set-doc-fn 'go-mode #'+godoc-at-point)
  (set-prettify-symbols 'go-mode
                        '(;;("func" . ?ƒ) ; WHY DOES THIS CAUSE A SEGFAULT
                          (":="   . ?←)))

  ;; fix up godoc-mode to be consistent with literally every other documentation mode
  (set-popup-buffer (rx bos "*godoc*" eos))
  (set-popup-buffer (rx bos "*godoc " (one-or-more anything) "*" eos))
  (add-hook 'godoc-mode-hook 'help-mode))

(req-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(req-package go-guru
  :commands (go-guru-describe go-guru-freevars go-guru-implements go-guru-peers
                              go-guru-referrers go-guru-definition go-guru-pointsto
                              go-guru-callstack go-guru-whicherrs go-guru-callers go-guru-callees
                              go-guru-expand-region)
  :general
  (:keymaps 'go-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "g"
            "" '(:ignore t :wk "guru")
            "<" #'go-guru-callers
            ">" #'go-guru-callees
            "c" #'go-guru-peers
            "d" #'go-guru-describe
            "f" #'go-guru-freevars
            "i" #'go-guru-implements
            "p" #'go-guru-pointsto
            "r" #'go-guru-referrers)
  :config
  (set-popup-buffer (rx bos "*go-guru-output*" eos)))

(req-package gorepl-mode
  :commands (gorepl-run gorepl-run-load-current-file)
  :hook (go-mode . gorepl-mode)
  :general
  (:keymaps 'go-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "'" #'gorepl-run)
  (:keymaps 'gorepl-mode-map
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "r"
            "" '(:ignore t :wk "repl")
            "f" #'gorepl-run-load-current-file
            "h" '(gorepl-hydra/body :wk "hydra")
            "l" #'gorepl-eval-line
            "r" #'gorepl-eval-region)
  :config
  (set-popup-buffer (rx bos "*Go REPL*" eos)))

(req-package company-go
  :requires company
  :commands company-go
  :init
  (setq command-go-gocode-command "gocode")
  (when (executable-find command-go-gocode-command)
    (set-company-backends 'go-mode 'company-go)))

(req-package flycheck-gometalinter
  :disabled t
  :requires flycheck
  :commands flycheck-gometalinter-setup
  :hook (flycheck-mode . flycheck-gometalinter-setup)
  :init
  (setq flycheck-gometalinter-vendor t
        flycheck-gometalinter-disabled-linters '("gocyclo")))

(provide 'lang-go)
;;; lang-go.el ends here
