;;; expando.el --- Quickly expand macros for easy reading/checking
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: lisp
;; URL: https://github.com/davep/expando.el

;; expando.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; expando.el provides a simple tool for expanding macros into a different
;; window so they can be quickly and easily checked and read.
;;
;; NOTE: This code makes use of `elisp--preceding-sexp' from elisp-mode.
;; This is, obviously, considered to be an "internal" function. Which is a
;; bit of shame really as it's a very handy thing that I think we'd normally
;; want to be doing in elisp, when doing elisp-oriented things. So, rather
;; than reinvent the wheel, I'll risk calling an internal and worry later
;; when things get broken.

;;; Code:

;;;###autoload
(defun expando-macro (&optional all)
  "Attempt to expand the expression at `point'.

By default `macroexpand' is used. Prefix a call to this function
with \\[universal-argument] (or pass ALL as a non-nil value) to
have `maxroexpand-all' be used."
  (interactive "P")
  (let ((form (elisp--preceding-sexp)))
    (with-current-buffer-window "*Expando Macro*" nil nil
      (emacs-lisp-mode)
      (pp (funcall (if all #'macroexpand-all #'macroexpand) form)))))

(provide 'expando)

;;; expando.el ends here
