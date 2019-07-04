;;; pypath.el --- Get dotted path for Python defun -*- lexical-binding: t -*-
;; Copyright 2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: languages
;; URL: https://github.com/davep/pypath.el
;; Package-Requires: ((emacs "25"))

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
;; pypath.el is a quick-and-dirty tool to help me find the dotted path/name
;; of the current defun in a Python file. It's mostly useful when I want to
;; test a very specific unit test when working on a Django application. It
;; also assumes that pipenv is the way to handle things.
;;
;; It's highly unlikely to be useful to anyone else.

;;; Code:

(require 'python)
(require 'subr-x)

(defun pypath--find-manage (path route)
  "Look for manage.py.

PATH is where we've got left to look, and ROUTE is where we've
been. If manage.py is found the route to it will be returned."
  (if (file-exists-p (string-join (append (reverse path) (list "manage.py")) "/"))
      route
    (when path
      (pypath--find-manage (cdr path) (cons (car path) route)))))

(defun pypath--get-path ()
  "Get the full dotted path for the current Python code.

As a side-effect, various environmental checks are done and if
there is a problem an `error' is thrown. Rules include:

- Current buffer must be in `python-mode'
- Current buffer must have a non-nil `buffer-file-name'
- `python-info-current-defun' must return a non-nil value"
  (unless (derived-mode-p 'python-mode)
    (error "This is only designed for use with `python-mode'"))
  (unless (buffer-file-name)
    (error "Buffer must be associated with an actual file"))
  (let ((pyfun (python-info-current-defun)))
    (unless pyfun
      (error "No Python defun found around `point'"))
    (let* ((path (reverse (split-string (buffer-file-name) "/")))
           (module (file-name-sans-extension (pop path)))
           (pyfun (string-join (append (pypath--find-manage path nil) (list module pyfun)) ".")))
      pyfun)))

;;;###autoload
(defun pypath ()
  "Get hold of the dotted path of the current Python function.

As well as displaying it with `message', the path is also placed
into the kill ring with `kill-new'."
  (interactive)
  (when-let ((pyfun (pypath--get-path)))
    (kill-new pyfun)
    (message "%s" pyfun)))

;;;###autoload
(defun pypath-django-test ()
  "Get the full Django test command.

As well as displaying it with `message', the path is also placed
into the kill ring with `kill-new'."
  (interactive)
  (when-let ((pyfun (pypath--get-path))
             (test (format "pipenv run ./manage.py test -v2 %s" pyfun)))
    (kill-new test)
    (message "%s" test)))

(provide 'pypath)

;;; pypath.el ends here
