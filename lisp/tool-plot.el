;;; tool-plot.el --- GnuPlot support -*- lexical-binding: t -*-

;;; Commentary:
;; Thesis stuff.

;;; Code:

(eval-when-compile
  (require 'base-package))

;;;
;; Packages

(use-package gnuplot
  :mode "\\.gp$"
  :mode "\\.gnuplot$")

(provide 'tool-plot)
;;; tool-plot.el ends here
