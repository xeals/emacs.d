;;; feature-hydra.el --- Hydras for efficiency -*- lexical-binding: t -*-

;;; Commentary:
;; Multi-headed quickness.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(req-package hydra :demand t)
(autoload 'defhydra "hydra")

;;;
;; Hydras

(defhydra xeal/dotfile-hydra (:color teal)
  "Quick dotfile access."
  ("E" (find-file (concat user-emacs-directory "init.el")) "emacs")
  ("e" xeal/find-dotfiles "emacs")
  ("h" (xeal/counsel-open-config "herbstluftwm") "hlwm")
  ("z" (xeal/counsel-open-config "zsh") "zsh")
  ("/" (counsel-find-file xdg-config-home) "search")
  ("q" nil "cancel" :color blue))

(defhydra +org/nav-hydra
  (:color red :hint nil)
  "
      ^Navigation^          ^Subtrees^
      ----------------------------------
      _H_: up a level       ^        ^
      _j_: prev heading     _<_: demote
      _k_: next heading     _>_: promote
      _h_: prev same level
      _l_: next same level
      "
  ("H" outline-up-heading)
  ("h" org-backward-heading-same-level)
  ("j" org-next-visible-heading)
  ("k" org-previous-visible-heading)
  ("l" org-forward-heading-same-level)
  (">" org-promote-subtree)
  ("<" org-demote-subtree)
  ("<tab>" +org/toggle-fold "toggle-fold")
  ("q" nil "quit" :color blue))

(defhydra +org/open-notes (:color blue)
  "Notes"
  ("a" (open-course "3600") "Algorithms")
  ("c" (open-course "2310") "Concurrent Systems")
  ("o" (open-course "3300") "OSI")
  ("s" (open-course "3530") "Systems Engineering")
  ("t" (open-course "3500") "TechLauncher"))

;;;
;; Supporting functions

(defun open-course (file)
  (interactive)
  (find-file (concat xeal-uni-dir "/notes/" file ".org")))


(provide 'feature-hydra)
;;; feature-hydra.el ends here
