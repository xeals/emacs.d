;;; theme.el --- Theme configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Visual niceness.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

;;;
;; Theme

(straight-use-package
 '(agila-theme
   :type git
   :repo "https://gist.github.com/xeals/23ecee56bdaf14d5298a8dc15a0d8170"))
(after! agila-theme
  (custom-theme-set-faces
   'agila
   '(fringe ((t (:background "#1A1E24"))))
   '(ivy-posframe ((t :foreground "#cdd3df" :background "#1a1e24")))
   '(solaire-default-face ((t (:inherit default :background "#252b33"))))
   '(solaire-hl-line-face ((t (:inherit hl-line :background "#1a1e24"))))
   '(solaire-org-hide-face ((t (:foreground "#252b33"))))))

;; (load-theme xeal-theme t)

(require 'metropolis-theme)
(load-theme 'metropolis)
(let ((base00 (plist-get metropolis-colors :base00))
      (base01 (plist-get metropolis-colors :base01))
      (base03 (plist-get metropolis-colors :base03))
      (base05 (plist-get metropolis-colors :base05))
      (base0A (plist-get metropolis-colors :base0A))
      (base0C (plist-get metropolis-colors :base0C))
      (base0E (plist-get metropolis-colors :base0E)))
  (custom-theme-set-faces
   'metropolis
   `(warning ((t :foreground ,base0A)))
   `(font-lock-keyword-face ((t :foreground ,base0C)))
   `(vertical-border ((t (:foreground ,base00 :background ,base00))))
   `(line-number ((t (:foreground ,base03 :background ,base00))))
   `(line-number-current-line ((t (:foreground ,base05 :background ,base00))))
   `(doom-modeline-bar ((t (:background ,base0C))))
   `(doom-modeline-buffer-file ((t (:foreground ,base0C :slant italic))))
   `(doom-modeline-buffer-path ((t (:foreground ,base0A :slant italic))))
   `(doom-modeline-evil-normal-state ((t (:foreground ,base0C :inherit bold))))
   `(doom-modeline-evil-insert-state ((t (:foreground ,base0A :inherit bold))))
   `(doom-modeline-evil-visual-state ((t (:foreground ,base0E :inherit bold))))))

;; Relatively adaptable theming. Maybe move this to hl-todo's init
(let ((hl-warn (face-attribute 'warning :foreground))
      (hl-error (face-attribute 'error :foreground))
      (hl-info (face-attribute 'font-lock-function-name-face :foreground))
      (hl-success (face-attribute 'success :foreground)))
  (setq hl-todo-keyword-faces
        `(("HACK"       . ,hl-warn)
          ("HOLD"       . ,hl-warn)
          ("KLUDGE"     . ,hl-warn)
          ("TEMP"       . ,hl-warn)
          ("OKAY"       . ,hl-info)
          ("NOTE"       . ,hl-info)
          ("FAIL"       . ,hl-error)
          ("FIXME"      . ,hl-error)
          ("XXX+"       . ,hl-error)
          ("\\?\\?\\?+" . ,hl-error)
          ("TODO"       . ,hl-error)
          ("NEXT"       . ,hl-error)
          ("DONE"       . ,hl-success))))

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
