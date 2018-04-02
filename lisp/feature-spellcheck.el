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
  (prog-mode . flyspell-prog-mode)
  (text-mode . flyspell-mode)
  (message-mode . flyspell-mode))

(req-package flyspell-correct-ivy
  :general
  (:keymaps 'motion
            "]S" 'flyspell-correct-next
            "[S" 'flyspell-correct-previous))

(req-package flyspell-popup
  :hook
  (flyspell-mode . flyspell-popup-auto-correct-mode)
  :general
  (:keymaps 'flyspell-mode-map
            :states 'insert
            "C-=" #'flyspell-popup-correct)
  :init
  (setq flyspell-popup-correct-delay 0.5))

(provide 'feature-spellcheck)
;;; feature-spellcheck.el ends here
