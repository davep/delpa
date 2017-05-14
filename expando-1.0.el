;;; expando.el --- Quickly expand macros for easy reading/checking
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: lisp
;; URL: https://github.com/davep/expando.el

;; expando.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; expando.el provides a simple tool for expanding macros into a different
;; window so they can be quickly and easily checked and read.

;;; Code:

(defun expando-macro ()
  "Attempt to expand the expression at `point'.

The expansion is displayed in a help window.

By default `macroexpand' is used. Prefix a call to this function
with \\[universal-argument] to have `maxroexpand-all' be used."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (let ((form (read (current-buffer))))
      (with-help-window "*Expando Macro"
        (pp (if current-prefix-arg
                (macroexpand-all form)
              (macroexpand form)))))))

(provide 'expando)

;;; expando.el ends here
