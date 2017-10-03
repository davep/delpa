;;; become.el --- Tools for transforming a buffer.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/become.el
;; Package-Requires: ((cl-lib "0.5"))

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
;; become.el provides a set of interactive functions that allow for easy
;; transformation of the current buffer.

;;; Code:

;; Things we need:
(eval-when-compile
  (require 'cl-lib))

;;;###autoload
(defun become-dos-buffer ()
  "Turn the current buffer into a DOS buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

;;;###autoload
(defun become-unix-buffer ()
  "Turn the current buffer into a Unix buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

;;;###autoload
(defun become-mac-buffer ()
  "Turn the current buffer into a Mac buffer."
  (interactive)
  (set-buffer-file-coding-system 'mac))

;;;###autoload
(defun become-undosly ()
  "Strip current buffer of DOS line end markers."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (while (search-forward "\015" nil t)
      (replace-match "" nil nil))
    (setf (point) (point-min))
    (while (search-forward "\032" nil t)
      (replace-match "" nil nil))
    (when (called-interactively-p 'interactive)
      (message "Buffer is now sane"))))

;;;###autoload
(defun become-freshly-indented ()
  "Apply indentation to whole buffer."
  (interactive)
  (indent-region (point-min) (point-max) nil))

;;;###autoload
(defun become-freshly-indented-no-tabs ()
  "Apply indentation to whole buffer and then untabify."
  (interactive)
  (become-freshly-indented)
  (untabify (point-min) (point-max)))

;;;###autoload
(defun become-free-of-trailing-whitespace ()
  "Remove all trailing whitespace from all lines in the current buffer.

Note that this function makes a point of not stripping the
trailing space from a signature separator line, or double
trailing spaces at the very end of a line in a markdown file."
  (interactive)
  (cl-flet ((is-sig-line ()
                         (save-excursion
                           (beginning-of-line)
                           (looking-at "^-- $")))
            (markdown-br-p ()
                           (save-excursion
                             (beginning-of-line)
                             (and (eq major-mode 'markdown-mode)
                                  (looking-at "^.+[^ ]  $")))))
    (save-excursion
      (setf (point) (point-min))
      (while (re-search-forward "[ \t\r]+$" nil t)
        (unless (or (is-sig-line) (markdown-br-p))
          (replace-match "" nil nil))))))

(provide 'become)

;;; become.el ends here
