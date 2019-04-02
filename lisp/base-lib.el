;;; base-lib.el --- Library functions -*- lexical-binding: t -*-

;;; Commentary:
;; Custom functions and helpers and stuff. Also stuff from doom-emacs and github.com/terlar.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(defvar-local documentation-function nil
  "Function to use for documentation lookups.")

;;;
;; Packages

;; Add multiple hooks at once
(use-package add-hooks
  :demand t)

;;;
;; Setup

;;;###autoload
(defmacro set-doc-fn (modes function)
  "Set MODES documentation FUNCTION using `documentation-function'."
  `(let* ((modes (if (listp ,modes) ,modes (list ,modes)))
          (fn-name (intern (format "set-doc-fn--%s" (mapconcat #'symbol-name modes "-")))))
     (defalias fn-name
       (lambda () (setq documentation-function ,function)))
     (add-hooks-pair modes fn-name)))

(defun documentation-at-point ()
  "Run the documentation lookup command defined by `documentation-function'."
  (interactive)
  (if (commandp documentation-function)
      (call-interactively documentation-function)
    (message (format "Documentation function not defined for %s" major-mode))))

(defmacro set-company-backends (mode &rest backends)
  "For MODE add BACKENDS to buffer-local version of `company-backends'."
  `(with-eval-after-load 'company
     (add-hooks-pair
      ,mode
      (lambda ()
        (make-variable-buffer-local 'company-backends)
        (dolist (backend (list ,@(reverse backends)))
          (cl-pushnew backend company-backends :test #'equal))))))

(defun set-popup-buffer (&rest buffers)
  "Display BUFFERS as popup."
  (dolist (buffer buffers)
    (cl-pushnew `(,buffer
                  (display-buffer-reuse-window
                   display-buffer-in-side-window)
                  (reusable-frames . visible)
                  (side            . bottom)
                  (window-height   . 0.4))
                display-buffer-alist :test #'equal)))

(defmacro set-prettify-symbols (modes symbols)
  "Set MODES prettified symbols to SYMBOLS."
  `(let* ((modes (if (listp ,modes) ,modes (list ,modes)))
          (fn-name (intern (format "set-prettify-symbols--%s" (mapconcat #'symbol-name modes "-")))))
     (defalias fn-name
       (lambda () (dolist (symbol ,symbols)
               (push symbol prettify-symbols-alist))))
     (add-hooks-pair modes fn-name)))

(defmacro add-graphic-hook (&rest forms)
  "Add FORMS as a graphical hook."
  `(if (daemonp)
       (add-hook 'after-make-frame-functions
                 (lambda (frame)
                   (with-selected-frame frame
                     (progn ,@forms))))
     (when (display-graphic-p)
       (add-hook 'after-init-hook
                 (lambda () (progn ,@forms))))))

(defmacro quiet! (&rest forms)
  "Run FORMS without making any noise."
  `(if xeal-debug-mode
       (progn ,@forms)
     (fset '+old-write-region-fn (symbol-function 'write-region))
     (cl-letf ((standard-output (lambda (&rest _)))
               ((symbol-function 'load-file) (lambda (file) (load file nil t)))
               ((symbol-function 'message) (lambda (&rest _)))
               ((symbol-function 'write-region)
                (lambda (start end filename &optional append visit lockname mustbenew)
                  (unless visit (setq visit 'no-message))
                  (when (fboundp '+old-write-region-fn)
                    (+old-write-region-fn
                     start end filename append visit lockname mustbenew))))
               (inhibit-message t)
               (save-silently t))
       ,@forms)))


(defmacro after! (feature &rest forms)
  "A smart wrapper around `with-eval-after-load'. Supresses warnings during
compilation."
  (declare (indent defun) (debug t))
  `(,(if (or (not (bound-and-true-p byte-compile-current-file))
             (if (symbolp feature)
                 (require feature nil :no-error)
               (load feature :no-message :no-error)))
         #'progn
       #'with-no-warnings)
    (with-eval-after-load ',feature ,@forms)))

(defun run-prog-mode-hooks ()
  "Runs all `prog-mode' hooks."
  (run-hooks 'prog-mode-hooks))

;;;
;; Buffers

;;;###autoload
(defun +kill-this-buffer ()
  "Kills the active buffer with no safeguards."
  (interactive)
  (let ((inhibit-message t))
    (kill-buffer (current-buffer))))

;;;###autoload
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              (cl-remove-if-not 'buffer-file-name (buffer-list)))))

;;;###autoload
(defun alternate-buffer (&optional window)
  "Switch back and forth between current and last buffer in the
current window."
  (interactive)
  (let ((current-buffer (window-buffer window)))
    ;; if no window is found in the windows history, `switch-to-buffer' will
    ;; default to calling `other-buffer'.
    (switch-to-buffer
     (cl-find-if (lambda (buffer)
                   (not (eq buffer current-buffer)))
                 (mapcar #'car (window-prev-buffers window))))))

;;;###autoload
(defun switch-messages-buffer ()
  "Switch to the *Messages* buffer."
  (interactive)
  (with-current-buffer (messages-buffer)
    (goto-char (point-max))
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun switch-scratch-buffer ()
  "Switch to the *scratch* buffer."
  (interactive)
  (let ((text (and (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))))
        (mode major-mode)
        (derived-p (derived-mode-p 'prog-mode 'text-mode))
        (scratch-buffer (get-buffer-create "*scratch*")))
    (with-current-buffer scratch-buffer
      (when (and (not (eq major-mode mode))
                 derived-p
                 (functionp mode))
        (funcall mode))
      (when text
        (when (> (buffer-size) 0)
          (goto-char (point-max))
          (newline))
        (insert text))
      (pop-to-buffer "*scratch*"))))

;; from magnars
;;;###autoload
(defun +rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (projectile-project-p)
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;;;###autoload
(defun +delete-file (filename &optional ask-user)
  "Remove specified file or directory.

Also kills associated buffer (if any exists) and invalidates
projectile cache when it's possible.

When ASK-USER is non-nil, user will be asked to confirm file
removal."
  (interactive "f")
  (when (and filename (file-exists-p filename))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer)))
    (when (or (not ask-user)
              (yes-or-no-p "Are you sure you want to delete this file? "))
      (delete-file filename)
      (when (projectile-project-p)
        (call-interactively #'projectile-invalidate-cache)))))

(defun +delete-file-confirm (filename)
  "Remove specified file or directory after users approval.

FILENAME is deleted using `+delete-file' function."
  (interactive "f")
  (funcall-interactively #'+delete-file filename t))

;; from magnars
;;;###autoload
(defun +delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (when (projectile-project-p)
          (call-interactively #'projectile-invalidate-cache))
        (message "File '%s' successfully removed" filename)))))

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
;;;###autoload
(defun +show-and-copy-buffer-filename ()
  "Show and copy the full path to the current file in the minibuffer."
  (interactive)
  ;; list-buffers-directory is the variable set in dired buffers
  (let ((file-name (or (buffer-file-name) list-buffers-directory)))
    (if file-name
        (message (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun maybe-push-exec-path (path)
  "Adds PATH to `exec-path', unless it is already present."
  (unless (member-ignore-case path exec-path)
    (setq exec-path (append exec-path (list path)))))

(provide 'base-lib)
;;; base-lib.el ends here
