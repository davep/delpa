;;; expando.el --- Quickly expand macros for easy reading/checking
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
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

(defun expando-macro (&optional all)
  "Attempt to expand the expression at `point'.

The expansion is displayed in a help window.

By default `macroexpand' is used. Prefix a call to this function
with \\[universal-argument] (or pass ALL as a non-nil value) to
have `maxroexpand-all' be used."
  (interactive "P")
  (save-excursion
    (beginning-of-defun)
    (let ((form (read (current-buffer))))
      (with-current-buffer (get-buffer-create "*Expando Macro*")
        (setf (buffer-string) "")
        (emacs-lisp-mode)
        (switch-to-buffer-other-window (current-buffer))
        (pp (if all (macroexpand-all form) (macroexpand form))
            (current-buffer))))))

(provide 'expando)

;;; expando.el ends here
