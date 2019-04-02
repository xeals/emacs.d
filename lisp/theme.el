;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Visual niceness.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Theme

;; FIXME This is broken for some reason.
;; (use-feature agila
;;   :config
;;   (custom-set-faces
;;    '(fringe ((t (:background "#1A1E24"))))
;;    '(ivy-posframe ((t :foreground "#cdd3df" :background "#1a1e24")))))
(straight-use-package
 '(agila-theme
   :type git
   :repo "https://gist.github.com/xeals/23ecee56bdaf14d5298a8dc15a0d8170"))
(after! agila-theme
  (custom-set-faces
   '(fringe ((t (:background "#1A1E24"))))
   '(ivy-posframe ((t :foreground "#cdd3df" :background "#1a1e24")))))

(load-theme xeal-theme t)

;;;
;; Typography

;; Host-based overrides
(pcase (system-name)
  ("shagaru"
   (setq xeal-font "Noto Mono"
         xeal-variable-pitch-font "Noto Serif"))
  ("baralyl"
   (setq xeal-font "Noto Sans Mono"
         xeal-variable-pitch-font "Noto Serif"
         xeal-font-height 95)))

(when (or (display-graphic-p) (daemonp))
  (with-demoted-errors "FONT ERROR: %s"
    (set-face-attribute 'default nil :height xeal-font-height :family xeal-font)
    (when xeal-variable-pitch-font
      (set-face-attribute 'variable-pitch nil :family xeal-variable-pitch-font :height 1.2))))

(require 'feature-ligatures)

(provide 'theme)
;;; theme.el ends here
