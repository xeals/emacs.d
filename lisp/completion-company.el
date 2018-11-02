;;; completion-company.el --- Auto-completion backend

;;; Commentary:
;; Modular in-buffer completion framework.

;;; Code:

(eval-when-compile
  (require 'base-keybinds)
  (require 'base-lib)
  (require 'base-package)
  (require 'base-vars))

;;;
;; Packages

(req-package company
  :demand t
  :hook
  (after-init . global-company-mode)
  :general
  (:keymaps 'company-mode-map
            :states 'insert
            "TAB" #'company-complete-common)
  (:keymaps 'company-active-map
            ;; Disable return
            [return]   'nil
            "RET"      'nil
            ;; Abort company instead of insert mode
            ;; [escape]  #'company-abort
            "C-g"     #'company-abort
            ;; Complete the common part then cycle
            ;; [tab]     #'company-complete-common-or-cycle
            ;; "TAB"     #'company-complete-common-or-cycle
            [tab]     #'company-complete
            "TAB"     #'company-complete
            [backtab] #'company-select-previous
            "S-TAB"   #'company-select-previous

            ;; Filter
            "C-s" #'company-filter-candidates
            "C-k" #'company-select-previous
            "C-p" #'company-select-previous
            "C-j" #'company-select-next
            "C-n" #'company-select-next

            ;; Rebind show location
            "C-w" 'nil
            "C-l" #'company-show-location)
  (:keymaps 'company-search-map
            "C-n" #'company-search-repeat-forward
            "C-p" #'company-search-repeat-backward)
  :init
  (setq company-idle-delay 0.1
        company-tooltip-limit 10

        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t

        ;; Less strict match requirements
        company-dabbrev-downcase nil
        company-dabbrev-ignore-case nil
        company-dabbrev-code-other-buffers t
        company-minimum-prefix-length 2
        company-require-match nil
        company-transformers '(company-sort-by-occurrence)

        company-global-modes
        '(not
          eshell-mode
          message-mode
          help-mode)

        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
        company-backends
        '((company-capf company-files :with company-yasnippet)
          (company-dabbrev-code
           company-keywords)
          company-dabbrev))
  :config
  (after! yasnippet (nconc company-backends '(company-yasnippet)))
  (after! evil (evil-declare-change-repeat 'company-complete)))

(req-package company-statistics
  :hook (company-mode . company-statistics-mode)
  :init
  (setq company-statistics-file (x/cache "company-stats-cache.el")))

(req-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode)
  :general
  (:keymaps 'company-active-map
            "C-h" #'company-quickhelp-manual-begin)
  :init
  (setq company-quickhelp-delay nil
        company-quickhelp-color-foreground "#e5e6e6"
        company-quickhelp-color-background "#424242"))

(req-package company-posframe
  :after company
  :hook (company-mode . company-posframe-mode))

;;;
;; Autoloads

(autoload 'company-capf         "company-capf")
(autoload 'company-dabbrev      "company-dabbrev")
(autoload 'company-dabbrev-code "company-dabbrev-code")
(autoload 'company-elisp        "company-elisp")
(autoload 'company-files        "company-files")
(autoload 'company-ispell       "company-ispell")
(autoload 'company-keywords     "company-keywords")
(autoload 'company-keywords     "company-keywords")
(autoload 'company-yasnipppet   "company-yasnipppet")

(provide 'completion-company)
;;; completion-company.el ends here
