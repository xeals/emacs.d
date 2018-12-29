;;; base-modeline.el --- Status bar -*- lexical-binding: t -*-

;;; Commentary:
;; Master of the state.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-lib))

;;;
;; Settings

(req-package all-the-icons
  :commands (all-the-icons-octicon all-the-icons-faicon all-the-icons-material))

(req-package doom-modeline
  :demand t
  :hook (after-init . doom-modeline-init)
  :init
  (setq doom-modeline-height 30
        doom-modeline-bar-width 3
        doom-modeline-buffer-file-name-style 'relative-to-project
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-minor-modes nil
        doom-modeline-lsp t))

(provide 'base-modeline)
;;; base-modeline.el ends here
