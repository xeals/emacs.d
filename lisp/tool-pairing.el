;;; tool-pairing.el --- Pair programming -*- lexical-binding: t -*-

;;; Commentary:
;; Floobits and maybe some other tools eventually.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(req-package floobits
  :commands (floobits-join-workspace
             floobits-share-dir-private
             floobits-share-dir-public
             floobits-leave-workspace
             floobits-summon
             floobits-follow-mode-toggle
             floobits-clear-highlights))

(provide 'tool-pairing)
;;; tool-pairing.el ends here
