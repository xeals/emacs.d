;;; feature-hydra.el --- Hydra configuration -*- lexical-binding: t -*-

;;; Commentary:
;; Multi-headed beasts to make life easier.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(req-package hydra :demand t)

(autoload 'defhydra "hydra")

;;;
;; Functions

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

(provide 'feature-hydra)
;;; feature-hydra.el ends here
