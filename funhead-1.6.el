;;; funhead.el --- Insert my standard function headers -*- lexical-binding: t -*-
;; Copyright 2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.6
;; Keywords: convenience
;; URL: https://github.com/davep/funhead.el
;; Package-Requires: ((emacs "24"))

;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; funhead.el is a simple tool that inserts my style of function header, to
;; help break up code. This likely won't be to anyone else's taste. This is
;; just for my own use.

;;; Code:

(defun funhead--format ()
  "Return the correct `funhead' format for the current buffer."
  (cond ((derived-mode-p 'python-mode 'sh-mode 'makefile-gmake-mode
                         'yaml-mode 'perl-mode 'julia-mode)
         (lambda ()
           (insert (concat (make-string 78 ?#) "\n# "))))
        ((derived-mode-p 'emacs-lisp-mode 'lisp-mode)
         (lambda ()
           (insert (concat (make-string 78 ?\;) "\n;; "))))
        ((derived-mode-p 'js-mode 'css-mode)
         (lambda ()
           (insert "/**\n * \n */")
           (next-line -1)))))

;;;###autoload
(defun funhead ()
  "Insert a function header on the line where `point' is."
  (interactive)
  (let ((header (funhead--format)))
    (when header
      (beginning-of-line)
      (insert "\n")
      (next-line -1)
      (funcall header))))

(provide 'funhead)

;;; funhead.el ends here
