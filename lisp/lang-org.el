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
       (org-toggle-latex-fragment))

      (`link
       (let ((path (org-element-property :path (org-element-lineage context '(link) t))))
         (if (and path (image-type-from-file-name path))
             (+org/refresh-inline-images)
           (org-open-at-point))))

      (_ (+org/refresh-inline-images)))
    (set-window-start nil scroll-pt)))

;;;
;; Packages

(req-package org :pin org
  :ensure org-plus-contrib
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . +org-setup-babel)
  (org-mode . +org-setup-templates)
  (org-mode . +line-numbers-disable)
  :general
  ;; core
  (:keymaps 'org-mode-map :states 'insert :major-modes t
            "RET" #'evil-org-return)
  (:keymaps 'org-mode-map :states 'normal :major-modes t
            "RET" #'+org/dwim-at-point
            "<<"  #'org-metaleft
            ">>"  #'org-metaright)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            "'" #'org-edit-special
            "*" #'org-ctrl-c-star
            "," #'org-ctrl-c-ctrl-c
            "-" #'org-ctrl-c-minus
            "e" '(org-export-dispatch :wk "export")
            "w" '(org-wc-display :wk "count words")
            "v" '(+org/nav-hydra/body :wk "navigate"))
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "d"
            "" '(:ignore t :wk "clocks")
            "d" #'org-deadline
            "t" #'org-time-stamp
            "T" #'org-time-stamp-inactive)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "i"
            "" '(:ignore t :wk "insert")
            "d" #'org-insert-drawer
            "f" #'org-footnote-new
            "h" #'org-insert-heading
            "H" #'org-insert-heading-after-current
            "l" #'org-insert-link
            "p" #'org-set-property
            "t" #'org-table-create)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "t"
            "" '(:ignore t :wk "table")
            "a" #'org-table-align
            "n" #'org-table-create
            "r" #'org-table-recalculate
            "R" #'org-table-recalculate-buffer-tables)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "ti"
            "" '(:ignore t :wk "insert")
            "c" #'org-table-insert-column
            "h" #'org-table-insert-hline
            "r" #'org-table-insert-row)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "s"
            "" '(:ignore t :wk "trees")
            "s" #'org-sort
            "a" #'org-archive-subtree
            "r" #'org-refile)
  (:keymaps 'org-mode-map :major-modes t
            :states '(normal visual operator)
            :prefix xeal-localleader-key
            :infix "T"
            "" '(:ignore t :wk "toggles")
            "l" #'org-toggle-link-display
            "x" #'org-toggle-latex-fragment
            "i" #'org-toggle-inline-images
            "T" #'org-todo)
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
  (defun +org-setup-babel ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (go         . t)
       (rust       . t)
       (python     . t)
       (shell      . t))))
  (defun +org-is-agenda-file (filename)
    (cl-find (file-truename filename) org-agenda-files
             :key #'file-truename
             :test #'equal))
  (defun +org-setup-templates ()
    ;; python
    (add-to-list 'org-structure-template-alist
                 '("p" "#+BEGIN_SRC python :results output\n\n#+END_SRC"
                   "<src lang=\"?\">\n\n</src>"))
    ;; R
    (add-to-list 'org-structure-template-alist
                 '("r" "#+BEGIN_SRC R :results graphics file: assets/fig_?.png\n\n#+END_SRC"
                   "<src lang=\"?\">\n\n</src>")))
  :init
  (setq org-hide-leading-stars t             ; only show last star
        org-hide-emphasis-markers t          ; pretty links, etc.
        org-hide-block-overlays t
        org-fontify-quote-and-verse-blocks t ; different faces
        org-src-fontify-natively t           ; fancy src blocks
        org-pretty-entities t                ; for latex inserts
        org-ellipsis "…"                    ; for hidden entries
        org-tags-column -80                  ; align to 80 characters

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
  (defvaralias 'org-directory 'xeal-org-dir)

  (add-to-list 'recentf-exclude #'+org-is-agenda-file)

  (set-popup-buffer (rx bos "*Org Agenda*" eos))

  ;; Load LaTeX classes
  (after! ox-latex
    (setq
     ;; make LaTeX previews a bit larger and more consistent colouring and faces
     org-preview-latex-image-directory (concat xeal-cache-dir "org-latex/")
     org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
     org-format-latex-options
     (plist-put org-format-latex-options
                :background
                (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                    'default)
                                :background nil t))
     ;; more LaTeX
     org-latex-pdf-process '("latexmk -f -pdf %f" "latexmk -c %f")
     )
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

(req-package org-bullets
  :hook (org-mode . org-bullets-mode))

(req-package org-wc
  :commands (org-word-count org-wc-count-subtrees org-wc-display org-wc-remove-overlays))

(req-package org-variable-pitch
  :el-get t :ensure nil
  :hook (org-mode . org-variable-pitch-minor-mode))

(req-package org-ref
  :requires helm
  :general
  (:keymaps 'org-mode-map
            :prefix xeal-localleader-key
            :infix "i"
            "c" #'org-ref-helm-insert-cite-link)
  :init
  (setq reftex-default-bibliography `(,(expand-file-name "references.bib" xeal-uni-dir))))

(req-package ox-dnd
  :el-get t :ensure nil)

(req-package ob-rust)
(req-package ob-go)

(provide 'lang-org)
;;; lang-org.el ends here
