;;; lang-org.el --- Org-mode support -*- lexical-binding: t -*-

;;; Commentary:
;; Org-mode is the other reason to use Emacs.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Functions

(defun +org/toggle-fold ()
  "Toggle the local fold at the point (as opposed to cycling
through all levels with `org-cycle'). Also:

  + If in a babel block, removes result blocks.
  + If in a table, realign it, if necessary."
  (interactive)
  (save-excursion
    (org-beginning-of-line)
    (cond ((org-at-table-p)
           (org-table-align))
          ((org-in-src-block-p)
           (org-babel-remove-result)
           (org-hide-block-toggle))
          ((org-at-heading-p)
           (outline-toggle-children))
          ((org-at-item-p)
           (let ((window-beg (window-start)))
             (org-cycle)
             (set-window-start nil window-beg))))))

(defun +org/refresh-inline-images ()
  "Refresh image previews in the current heading/tree."
  (interactive)
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.
If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote definition: jump to the footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((scroll-pt (window-start))
         (context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (`headline
       (cond ((org-element-property :todo-type context)
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done) 'todo 'done)))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             (t
              (org-remove-latex-fragment-image-overlays)
              (org-toggle-latex-fragment '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-definition
       (goto-char (org-element-property :post-affiliated context))
       (call-interactively #'org-footnote-action))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
         (if (and path (image-type-from-file-name path))
             (+org/refresh-inline-images)
           (org-open-at-point))))

      (_ (+org/refresh-inline-images)))
    (set-window-start nil scroll-pt)))

(defun org-toggle-emphasis-markers ()
  (interactive)
  (setq org-emphasis-markers (not org-emphasis-markers)))

;;;
;; Packages

(use-feature org
  :ensure org-plus-contrib
  :hook
  (org-mode . hl-todo-mode)
  (org-mode . org-indent-mode)
  (org-mode . +line-numbers-disable)
  (org-mode . +org-setup-babel)
  (org-mode . +org-setup-templates)
  :general
  ;; core
  (:keymaps 'org-mode-map :states 'insert :major-modes t
   "RET" #'evil-org-return)

  (:keymaps 'org-mode-map :states 'normal :major-modes t
   "RET" #'+org/dwim-at-point
   "<<"  #'org-metaleft
   ">>"  #'org-metaright
   "j" #'evil-next-visual-line
   "k" #'evil-previous-visual-line)

  (:keymaps 'org-mode-map :major-modes t
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "'" #'org-edit-special
   "*" #'org-ctrl-c-star
   "," #'org-ctrl-c-ctrl-c
   "-" #'org-ctrl-c-minus
   "e" '(org-export-dispatch :wk "export")
   "P" `(+org/export-and-open-pdf :wk "open pdf")
   "w" '(org-wc-display :wk "count words")
   "v" '(+org/nav-hydra/body :wk "navigate")
   "=" #'org-align-all-tags
   "C-t" #'org-babel-tangle

   "c" '(:ignore t :wk "timekeeping")
   "cc" #'org-clock-in
   "cC" #'org-clock-cancel
   "cd" #'org-clock-display
   "cg" #'org-clock-goto
   "ci" #'org-clock-in
   "co" #'org-clock-out
   "cR" #'org-clock-report
   "cu" #'org-clock-update-time-maybe

   "d" '(:ignore t :wk "clocks")
   "dd" #'org-deadline
   "ds" #'org-schedule
   "dt" #'org-time-stamp
   "dT" #'org-time-stamp-inactive

   "i" '(:ignore t :wk "insert")
   "id" #'org-insert-drawer
   "if" #'org-footnote-new
   "ih" #'org-insert-heading
   "iH" #'org-insert-heading-after-current
   "il" #'org-insert-link
   "ip" #'org-set-property
   "it" #'org-table-create

   "t" '(:ignore t :wk "table")
   "ta" #'org-table-align
   "tn" #'org-table-create
   "tr" #'org-table-recalculate
   "tR" #'org-table-recalculate-buffer-tables

   "ti" '(:ignore t :wk "insert")
   "tic" #'org-table-insert-column
   "tih" #'org-table-insert-hline
   "tir" #'org-table-insert-row

   "s" '(:ignore t :wk "trees")
   "ss" #'org-sort
   "sa" #'org-archive-subtree
   "sr" #'org-refile

   "T" '(:ignore t :wk "toggles")
   "Ti" #'org-toggle-inline-images
   "Te" #'org-toggle-emphasis-markers
   "Tl" #'org-toggle-link-display
   "Tp" #'org-toggle-pretty-entities
   "Tx" #'org-latex-preview
   "TX" #'org-clear-latex-preview
   "TT" #'org-todo)

  ;; org-agenda
  (:keymaps 'org-agenda-mode-map :states 'emacs
   "j" #'org-agenda-next-item
   "k" #'org-agenda-previous-item)

  ;; org-src
  (:keymaps 'org-src-mode :states 'normal :definer 'minor-mode
   "ZZ" #'org-edit-src-exit)
  (:keymaps 'org-src-mode :definer 'minor-mode
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   "," #'org-edit-src-exit
   "k" #'org-edit-src-abort)

  :preface
  (defun +org/export-and-open-pdf ()
    (interactive)
    (let ((pdf-name (org-latex-export-to-pdf nil)))
      (call-process-shell-command (format "%s %s &" xeal-pdf-program-name pdf-name) nil 0)))
  (defun +org-setup-babel ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((C          . t)
       (emacs-lisp . t)
       (go         . t)
       (gnuplot    . t)
       (python     . t)
       (rust       . t)
       (shell      . t))))
  (defun +org-is-agenda-file (filename)
    (cl-find (file-truename filename) org-agenda-files
             :key #'file-truename
             :test #'equal))
  (defun +org-setup-templates ()
    ;; Structure of this changed in 26.2 with org-mode templating changes.
    (when (version<= "26.2" emacs-version)
      (add-to-list 'org-structure-template-alist
                   '("p" . "src python :results output"))
      (add-to-list 'org-structure-template-alist
                   '("r" . "src R :results graphics file: assets/fig_?.png"))
      (require 'org-tempo)))
  :init
  (setq org-hide-leading-stars nil           ; only show last star
        org-hide-emphasis-markers nil        ; pretty links, etc.
        org-hide-block-overlays t
        org-fontify-quote-and-verse-blocks t ; different faces
        org-src-fontify-natively t           ; fancy src blocks
        org-pretty-entities nil              ; for latex inserts
        org-ellipsis " › "                   ; for hidden entries
        org-tags-column -75                  ; align to 80 characters

        org-fontify-done-headline t
        org-fontify-whole-heading-line t

        ;;        highlight latex stuff
        org-highlight-latex-and-related '(native entities)

        ;;; Getting Things Done
        ;; set file handlers
        org-agenda-files (list gtd/inbox-file gtd/main-file)
        org-default-notes-file gtd/inbox-file

        ;; customise agenda
        org-agenda-restore-windows-after-quit t
        org-agenda-span 14
        org-agenda-custom-commands
        '(("A" "Assignment work" tags-todo "ass"
           ((org-agenda-overriding-header "Assignments")))
          ("D" "Dev stuff" tags-todo "dev"
           ((org-agenda-overriding-header "Development"))))

        ;; refiling
        org-refile-targets `((,gtd/main-file :maxlevel . 2)
                             (,gtd/someday-file :level . 1))

        ;; todo stuff
        org-todo-keywords '((sequence "TODO(t)" "ICEBOX(i)" "BACKLOG(l)"
                                      "STARTED(s)" "BLOCKED(b)" "|"
                                      "DONE(d)" "CANCELLED(c)"))
        org-log-done 'time
        org-log-done-with-time t)
  :config
  ;; FIXME was (defvaralias 'org-directory 'xeal-org-dir)
  (setq org-directory xeal-org-dir)

  (after! base-editor
    (add-to-list 'recentf-exclude #'+org-is-agenda-file))

  ;; window padding
  (progn
    (setq header-line-format " ")
    (setq left-margin-width 2)
    (setq right-margin-width 2)
    (set-window-buffer nil (current-buffer)))

  ;; Load LaTeX classes
  (after! ox-latex
    (require 'ox-beamer)
    ;; FIXME This is broken for some reason.
    ;; (use-package ox-dnd
    ;;   :straight (ox-dnd
    ;;              :type git
    ;;              :host github
    ;;              :repo "xeals/emacs-org-dnd"))
    (straight-use-package
     '(ox-dnd
       :type git
       :host github
       :repo "xeals/emacs-org-dnd"))
    (require 'ox-dnd)

    (setq org-latex-listings t)
    (add-to-list 'org-latex-packages-alist '("" "listings"))
    (add-to-list 'org-latex-packages-alist '("" "color"))

    (setq
     ;; make LaTeX previews a bit larger and more consistent colouring and faces
     org-preview-latex-image-directory (x/cache "org-latex/")
     org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))
     ;; more LaTeX
     org-latex-pdf-process '("latexmk -shell-escape -bibtex -pdf -f %f"))
    (unless (assoc "assignment" org-latex-classes)
      (add-to-list 'org-latex-classes
                   '("assignment" "\\documentclass[11pt]{article}
[DEFAULT-PACKAGES]
\\usepackage[nottoc,numbib]{tocbibind}
\\usepackage{listings}
\\usepackage{natbib}
\\usepackage{svg}
\\setcitestyle{authoryear}
\\hypersetup{pdfborder=0 0 0}"
                     ("\\section{%s}" . "\\section*{%s}")
                     ("\\subsection{%s}" . "\\subsection*{%s}")
                     ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                     ("\\paragraph{%s}" . "\\paragraph*{%s}")
                     ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :init (setq org-bullets-bullet-list '("※")))

(use-package org-wc
  :commands (org-word-count
             org-wc-count-subtrees
             org-wc-display
             org-wc-remove-overlays))

(use-package org-variable-pitch
  :general
  (:keymaps 'org-mode-map
   :states '(normal visual operator)
   :prefix xeal-localleader-key
   :infix "T"
   "v" #'org-variable-pitch-minor-mode)
  :init
  (setq org-variable-pitch-fixed-font xeal-font))

(use-package ob-rust)
(use-package ob-go)

(provide 'lang-org)
;;; lang-org.el ends here
