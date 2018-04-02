;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Visual niceness.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Theme

(req-package pencil-theme
  :el-get t :ensure t
  :force t
  :demand t
  :disabled
  :init
  (setq pencil/higher-contrast-ui t
        pencil/italics t
        pencil/nebula t
        pencil/variable-family xeal-variable-pitch-font
        pencil/hide-org-block-headers nil)
  :config
  (load-theme 'pencil-dark t))

(req-package apropospriate-theme
  :demand t
  :force t)

(req-package panda-theme
  :demand t
  :force t)

(load-theme 'apropospriate-dark t)

;; (load-theme xeal-theme t)

;;;
;; Typography

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height xeal-font-height :family xeal-font)
    ;; (when xeal-variable-pitch-font
      ;; (set-face-attribute 'variable-pitch nil :family xeal-variable-pitch-font :height 1.2))
    ))

(provide 'theme)
;;; theme.el ends here
