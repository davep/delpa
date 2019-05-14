;;; end-it.el --- Add an "ends here" marker to a file -*- lexical-binding: t -*-
;; Copyright 2018-2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.14
;; Keywords: convenience
;; URL: https://github.com/davep/end-it.el
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
;; end-it.el is a simple tool that provides a command for quickly adding a
;; "foo.xxx ends here" marker to the end of the current buffer. The format
;; used to add it takes into account the language in use and attempts to use
;; the correct comment style.

;;; Code:

(defun end-it--format ()
  "Return the correct `end-it' format for the current buffer."
  (cond ((derived-mode-p 'lisp-mode 'emacs-lisp-mode 'clojure-mode)
         ";;; %s")
        ((derived-mode-p 'python-mode 'ruby-mode 'makefile-gmake-mode
                         'makefile-bsdmake-mode 'sh-mode 'restclient-mode
                         'conf-mode 'yaml-mode 'dockerfile-mode 'perl-mode
                         'gitconfig-mode 'julia-mode 'fish-mode)
         "### %s")
        ((derived-mode-p 'c-mode 'css-mode 'js-mode)
         "/* %s */")
        ((derived-mode-p 'rust-mode)
         "// %s")
        ((derived-mode-p 'markdown-mode)
         "[//]: # (%s)")
        ((derived-mode-p 'web-mode)
         (cond ((string= (symbol-value 'web-mode-engine) "django")
                "{# %s #}")
               (t
                "<!-- %s -->")))
        ((derived-mode-p 'haskell-mode)
         "-- %s")
        (t
         "%s")))

(defun end-it--previous-line-empty-p ()
  "Is the line before this one empty?"
  (save-excursion
    (beginning-of-line)
    (forward-line -1)
    (eolp)))

;;;###autoload
(defun end-it ()
  "Add a end-of-file marker to the current buffer.

As a side effect `point' will be moved to the end of the
buffer (this is a deliberate design decision, as I want to be
able to see that the addition worked okay and makes sense)."
  (interactive "*")
  (if (buffer-file-name)
      (let ((file (file-name-nondirectory (buffer-file-name)))
            (format (end-it--format)))
        (setf (point) (point-max))
        (unless (bolp)
          (insert "\n"))
        (unless (end-it--previous-line-empty-p)
          (insert "\n"))
        (insert (format format (format "%s %s" file "ends here")))
        (insert "\n"))
    (error "It only makes sense to end-it in buffers that are related to a file")))

(provide 'end-it)

;;; end-it.el ends here
