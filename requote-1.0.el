;;; requote.el --- Toggle quotes around a string. -*- lexical-binding: t -*-
;; Copyright 2018 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience, string, quotes
;; URL: https://github.com/davep/requote.el
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
;; requote.el provides a simple command for toggling the quote style around
;; a string in a buffer. It's designed to quickly and easily help turn a
;; "string" into a 'string', or a 'string' into a "string", depending on
;; what it finds.

;;; Code:

(defun requote-set-char (start end char)
  "Set characters at START and END to CHAR."
  (setf (buffer-substring start (1+ start)) char)
  (setf (buffer-substring (1- end) end) char))

;;;###autoload
(defun requote (start end)
  "Toggle quotes on a string bounded by START and END.

Designed as a simple tool for quickly switching a string from one
style of quote to another."
  (interactive "*r")
  (if (and start end (= (char-after start) (char-after (1- end))))
      (let ((char (string (char-after start))))
        (cond ((string= char "\"")
               (requote-set-char start end "'"))
              ((string= char "'")
               (requote-set-char start end "\""))
              (t
               (error "That region doesn't bound quotes"))))
    (error "Please mark a region that starts and ends with the quotes to change")))

(provide 'requote)

;;; requote.el ends here
