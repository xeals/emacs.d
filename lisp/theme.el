;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Visual niceness.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Theme

(req-package base16-agila-theme
  :el-get t :ensure nil
  :require base16-theme)

(req-package pencil-theme
  :el-get t :ensure t
  :force t :demand t
  :disabled t
  :init
  (setq pencil/higher-contrast-ui t
        pencil/italics t
        pencil/nebula t
        pencil/variable-family xeal-variable-pitch-font
        pencil/hide-org-block-headers nil)
  :config
  (load-theme 'pencil-dark t))

(req-package doneburn-theme
  :el-get t :ensure t
  :disabled t)

(req-package apropospriate-theme
  :disabled t)
(req-package panda-theme
  :disabled t)
(req-package challenger-deep-theme
  :disabled t)

(load-theme xeal-theme t)

;; (req-package circadian
;;   :demand t :force t
;;   :disabled t
;;   :config
;;   (setq calendar-latitude -35.3
;;         calendar-longitude 149.1
;;         circadian-themes '((:sunrise . apropospriate-light)
;;                            (:sunset  . apropospriate-dark)))
;;   (circadian-setup))

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
