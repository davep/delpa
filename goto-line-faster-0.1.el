;;; goto-line-faster.el --- Start going to a line number quickly -*- lexical-binding: t -*-
;; Copyright 2020 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/davep/goto-line-faster.el
;; Package-Requires: ((emacs "24") (bind-key "2.0"))

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
;; goto-line-faster.el provides a utility function that binds M-g followed
;; by a number (1 through 9) to an invocation of goto-line with the number
;; pre-populated in the input in the minibuffer.
;;
;; I wrote this because I've never liked 'M-g g' as a way to go to a line
;; number, but I'd like to use some of the other M-g-prefixed key bindings
;; too. This pretty much gives me the best of both worlds.

;;; Code:

(require 'bind-key)

(defun goto-line-faster--maker (prefix)
  "Create an interactive function to start going to a line.

The line number that we'll go to will start with PREFIX."
  (let ((name (intern (format "goto-line-faster-%s" prefix))))
    (prog1
        (defalias name
          (lambda ()
            (interactive)
            (setq unread-command-events (listify-key-sequence (number-to-string prefix)))
            (command-execute #'goto-line)))
      (setf (get name 'function-documentation) (format "Go to a line whose number starts with %s" prefix)))))

(dotimes (n 9)
  (bind-key (format "M-g %s" (1+ n)) (goto-line-faster--maker (1+ n))))

(provide 'goto-line-faster)

;;; goto-line-faster.el ends here
