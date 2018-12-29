;;; base-ui.el --- UI configuration -*- lexical-binding: t -*-

;;; Commentary:
;; We need to look pretty.

;;; Code:

(eval-when-compile
  (require 'base-vars)
  (require 'base-package))

(setq-default
 bidi-display-reordering nil            ; disable bidirectional text for tiny performance boost
 cursor-in-non-selected-windows nil     ; hide cursor in nonactive windows
 highlight-nonselected-windows nil      ; no highlight either
 image-animate-loop t                   ; loop images
 uniquify-buffer-name-style #'forward   ; unique/buffer/names
 visible-bell nil                       ; fuck the bell
 ring-bell-function #'ignore            ; fuck the bell.
 display-line-number-width 3            ; display-line, when it gets enabled
 ;; window title
 frame-title-format '(multiple-frames ("%b - " invocation-name) invocation-name)
 ;; Help
 help-window-select t                   ; always focus help window
 show-help-function nil                 ; hide :help-echo text
 ;; Scrolling
 scroll-conservatively 1001             ; "smooth scroll"
 scroll-margin 0                        ; slight margin in scrolling
 scroll-preserve-screen-position t      ; move cursor on less than full screens only
 )

;; Visual line wrapping
(add-hooks-pair '(text-mode
                  prog-mode
                  Man-mode)
                'visual-line-mode)

;;;
;; Packages

;; Centred window
(req-package centered-window
  :commands centered-window-mode
  :init
  (setq cwm-centered-window-width 80))

;; Highlight the current line
(req-package hl-line
  :commands (hl-line-mode global-hl-line-mode)
  ;;:hook (after-init . global-hl-line-mode)
  )

;; Colour the cursor based on the foreground
(req-package smart-cursor-color
  :commands (smart-cursor-color-mode)
  :hook (after-init . smart-cursor-color-mode))

;; Highlight the current line number
(req-package hlinum
  :commands (hlinum-activate hlinum-deactivate))

;; Use line numbers
(req-package nlinum
  :disabled t
  :commands (nlinum-mode global-nlinum-mode))

;; Use line numbers
(req-package linum
  :commands (linum-mode global-linum-mode)
  :hook
  (prog-mode . +line-numbers-enable)
  (text-mode . +line-numbers-enable)
  (conf-mode . +line-numbers-enable)
  )

;; Highlight hex colours
(req-package rainbow-mode
  :mode "-theme\\.el$"
  :commands rainbow-mode
  :init
  (setq rainbow-html-colors nil))

;; Highlight variables dynamically
(req-package color-identifiers-mode
  :commands (color-identifiers-mode
             global-color-identifiers-mode
             color-identifiers:refresh)
  :init
  (setq color-identifiers:num-colors 10)
  :hook ((lisp-mode emacs-lisp-mode) . color-identifiers-mode))

;; Highlight functions dynamically
(req-package rainbow-identifiers
  :diminish rainbow-identifiers-mode
  :commands rainbow-identifiers-mode
  :functions rainbow-identifiers-cie-l*a*b*-choose-face
  :init
  (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
        rainbow-identifiers-cie-l*a*b*-saturation 65
        rainbow-identifiers-cie-l*a*b*-lightness 45))

;; Highlight TODO inside comments and strings
(req-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :general
  (:keymaps 'motion
            "]t" #'hl-todo-next
            "[t" #'hl-todo-previous)
  :init
  ;; pencil-theme
  (when (or
         (eq xeal-theme 'pencil)
         (eq xeal-theme 'pencil-dark))
    (let ((hl-y "#f3e430")                ; yellow
          (hl-o "#d75f5f")                ; orange
          (hl-r "#e32791")                ; light-red
          (hl-g "#5fd7a7")                ; light-green
          (hl-b "#c30071")                ; dark-red
          (hl-p "#6855de"))               ; dark-purple
      (setq hl-todo-keyword-faces
            `(("HOLD"   . ,hl-y)
              ("HACK"   . ,hl-y)
              ("KLUDGE" . ,hl-y)
              ("NOTE"   . ,hl-y)
              ("TODO"   . ,hl-o)
              ("FIXME"  . ,hl-o)
              ("XXX"    . ,hl-o)
              ("XXXX"   . ,hl-o)
              ("???"    . ,hl-o)
              ("NEXT"   . ,hl-o)
              ("PROG"   . ,hl-g)
              ("OKAY"   . ,hl-g)
              ("DONE"   . ,hl-g)
              ("DONT"   . ,hl-p)
              ("THEM"   . ,hl-r)
              ("FAIL"   . ,hl-b))))))

;; Colour matching delimiters
(req-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight matching delimiters
(req-package paren
  :defer 2
  :init
  (setq show-paren-delay 0.1
        show-paren-highlight-openparen t
        show-paren-when-point-inside-paren t)
  :config
  (show-paren-mode 1))

;; Major mode for editing source code
(req-package prog-mode
  :ensure nil
  :config
  (set-prettify-symbols 'prog-mode
                        '(("lambda" . ?λ)
                          ("/=" . ?≠)
                          ("!=" . ?≠)
                          (">=" . ?≥)
                          ("<=" . ?≤)))
  (global-prettify-symbols-mode 1))

(req-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-character 124))

;;;
;; Autoloads

(defvar-local +line-numbers-toggle nil
  "Whether or not line numbers are enabled.

Toggle using `+line-numbers-toggle'.")

;;;###autoload
(defun +color-identifiers-toggle ()
  "Toggle identifier colourisation."
  (interactive)
  (if (or (bound-and-true-p color-identifiers-mode) (bound-and-true-p rainbow-identifiers-mode))
      (progn
        (color-identifiers-mode 0)
        (rainbow-identifiers-mode 0))
    (rainbow-identifiers-mode 1)))

;;;###autoload
(defun +line-numbers-enable ()
  "Enables the display of line numbers using `display-line-numbers'(in Emacs 26+) or `nlinum-mode'."
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers t)
    (linum-mode 1))
  (hlinum-activate)
  (setq +line-numbers-toggle t))

;;;###autoload
(defun +line-numbers-disable ()
  "Disable the display of line numbers."
  (if (boundp 'display-line-numbers)
      (setq display-line-numbers nil)
    (linum-mode -1))
  (hlinum-deactivate)
  (setq +line-numbers-toggle nil))

;;;###autoload
(defun +line-numbers-toggle ()
  "Toggles the display of line numbers."
  (interactive)
  (if +line-numbers-toggle
      (+line-numbers-disable)
    (+line-numbers-enable)))

(provide 'base-ui)
;;; base-ui.el ends here
