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

(require 'doom-modeline)

(provide 'base-modeline)
;;; base-modeline.el ends here
