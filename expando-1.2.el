;;; expando.el --- Quickly expand macros for easy reading/checking
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
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

(require 'thingatpt)

;;;###autoload
(defun expando-macro (&optional all)
  "Attempt to expand the expression at `point'.

By default `macroexpand' is used. Prefix a call to this function
with \\[universal-argument] (or pass ALL as a non-nil value) to
have `maxroexpand-all' be used."
  (interactive "P")
  (let ((form (read (thing-at-point 'list t))))
    (with-current-buffer-window "*Expando Macro*" nil nil
      (emacs-lisp-mode)
      (pp (funcall (if all #'macroexpand-all #'macroexpand) form)))))

(provide 'expando)

;;; expando.el ends here
