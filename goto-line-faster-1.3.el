;;; goto-line-faster.el --- Start going to a line number quickly -*- lexical-binding: t -*-
;; Copyright 2020-2023 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/goto-line-faster.el
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
;; goto-line-faster.el provides a utility function that binds M-g followed
;; by a number (1 through 9) to an invocation of goto-line with the number
;; pre-populated in the input in the minibuffer.
;;
;; I wrote this because I've never liked 'M-g g' as a way to go to a line
;; number, but I'd like to use some of the other M-g-prefixed key bindings
;; too. This pretty much gives me the best of both worlds.
;;
;; Please note that this version of the code is very different from the
;; original version, with <URL:https://github.com/phil-s> providing a much
;; cleaner approach.

;;; Code:

(defgroup goto-line-faster nil
  "Go to a line, faster."
  :group 'convenience
  :prefix "goto-line-faster-")

(defcustom goto-line-faster-goto-line-function #'goto-line
  "The function to call to go to a line."
  :type 'function
  :group 'goto-line-faster)

;;;###autoload
(defun goto-line-faster (_)
  "Slightly faster `goto-line' for those with particular muscle memory.

This command is designed to be bound to the goto prefix key
combination followed by the numbers 1 through 9, to give the
effect of simply hitting the goto prefix combination and starting
to type a line number. The result is that `goto-line' is called
with the minibuffer prepopulated with the relevant value.

This command is likely of little utility to anyone who doesn't
have this particular muscle memory.

If you wish to override the function that is actually called to
go to a line, set `goto-line-faster-goto-line-function'."
  (interactive "P")
  (when (and (<= ?1 last-command-event ?9) (not (numberp current-prefix-arg)))
    (push last-command-event unread-command-events))
  (call-interactively goto-line-faster-goto-line-function))

;; Set up quick goto-line bindings for line numbers that start with 1
;; through 9...
(dotimes (n 9)
  (global-set-key (kbd (format "M-g %s" (1+ n))) #'goto-line-faster))

(provide 'goto-line-faster)

;;; goto-line-faster.el ends here
