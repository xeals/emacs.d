;;; feature-hydra.el --- Hydras for efficiency -*- lexical-binding: t -*-

;;; Commentary:
;; Multi-headed quickness.

;;; Code:

(eval-when-compile
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(use-package hydra
  :demand t)
(autoload 'defhydra "hydra")

;;;
;; Hydras

(defhydra xeal/dotfile-hydra (:color blue)
  "Quick dotfile access."
  ("E" (find-file (concat user-emacs-directory "init.el")) "init.el")
  ("e" xeal/find-dotfiles "emacs")
  ("h" (xeal/counsel-open-config "herbstluftwm") "hlwm")
  ("z" (xeal/counsel-open-config "zsh") "zsh")
  ("/" (counsel-find-file xdg-config-home) "search"))

(defhydra +evil/nav-hydra
  (:color red :hint nil)
  "
^^Resizing^    ^^^^^Movement^
^^^^-----------^^^^----------
^^   _+_       ^^^     _k_
 _<_ _=_ _>_        _h_   _l_
^^   _-_       ^^^     _j_
"
  ("<" evil-window-decrease-width)
  (">" evil-window-increase-width)
  ("-" evil-window-decrease-height)
  ("+" evil-window-increase-height)
  ("=" balance-windows)
  ("h" evil-window-left)
  ("j" evil-window-up)
  ("k" evil-window-down)
  ("l" evil-window-right)
  ("q" nil "quit" :color blue))

(defhydra +org/nav-hydra
  (:color red :hint nil)
  "
 ^Navigation^          ^Subtrees
-----------------------------------
 _H_: up a level
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
  ("a" (open-course "COMP3600") "Algorithms")
  ("e" (open-course "ENGN3230") "Engineering Innovations")
  ("r" (find-file (concat xeal-uni-dir "/COMP4540/thesis.org")) "Research"))

(defhydra +uni/open-dir (:color blue)
  "Uni folders"
  ("a" (xeal/open-uni "COMP3600") "Algorithms")
  ("e" (xeal/open-uni "ENGN3230") "Engineering Innovations")
  ("r" (xeal/open-uni "COMP4540") "Research"))

;;;
;; Supporting functions

(defun open-course (file)
  (interactive)
  (find-file (concat xeal-uni-dir "/notes/" file ".org")))


(provide 'feature-hydra)
;;; feature-hydra.el ends here
