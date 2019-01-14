;;; feature-spellcheck.el --- Spellchecking -*- lexical-binding: t -*-

;;; Commentary:
;; I'm not American, dammit.

;;; Code:

(eval-when-compile
  (require 'base-lib)
  (require 'base-keybinds)
  (require 'base-package))

;;;
;; Packages

(req-package flyspell
  :hook
  ;; "<t> is undefined" [20190112]
  ;; (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  (message-mode . flyspell-mode))

(req-package flyspell-correct-ivy
  :general
  (:keymaps 'motion
            "]S" 'flyspell-correct-next
            "[S" 'flyspell-correct-previous))

;; Wishlist: flyspell-posframe
(req-package flyspell-popup
  :disabled t
  :hook
  (flyspell-mode . flyspell-popup-auto-correct-mode)
  :general
  (:keymaps 'flyspell-mode-map
            :states 'insert
            "C-=" #'flyspell-popup-correct)
  :init
  (setq flyspell-popup-correct-delay 0.5))

(req-package flyspell-correct-ivy
  :general
  (:keymaps 'flyspell-mode-map
            "C-;" #'flyspell-correct-wrapper))

(req-package writegood-mode
  :commands (writegood-mode
             writegood-grade-level
             writegood-reading-ease))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
