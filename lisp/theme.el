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
  :require base16-theme
  :config
  (custom-set-faces
   '(fringe ((t (:background "#1A1E24"))))
   '(ivy-posframe ((t :foreground "#cdd3df" :background "#1a1e24")))))

(load-theme xeal-theme t)

;;;
;; Typography

;; Host-based overrides
(let* ((s (shell-command-to-string "hostname"))
       ;; Taken from s.el :: s-trim-right
       (host (save-match-data
         (declare (pure t) (side-effect-free t))
         (if (string-match "[ \t\n\r]+\\'" s)
             (replace-match "" t t s)
           s))))
  (pcase host
    ("shagaru"
     (setq xeal-font "Noto Mono"
           xeal-variable-pitch-font "Noto Serif"))))

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height xeal-font-height :family xeal-font)
    (when xeal-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family xeal-variable-pitch-font :height 1.2))))

(require 'feature-ligatures)

(provide 'theme)
;;; theme.el ends here
