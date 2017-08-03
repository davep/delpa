;;; fscroll.el --- Make scroll-{up,down} go all the way.
;; Copyright 2002-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
;; Keywords: convenience
;; URL: https://github.com/davep/fscroll.el

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

;; Out of the box GNU Emacs only scrolls the buffer when using `scroll-down'
;; and `scroll-up' (which most people would have bound to PgDn and PgUp).
;; Once the first or last lines are visible you'll get start/end of buffer
;; errors if you try and hit those keys more. Many other editors will, when
;; that happens, move the cursor to the start or end of the buffer. This
;; code makes GNU Emacs do that too.

;;; Code:

(defadvice scroll-down (around full-scroll-down activate)
  "Have `scroll-down' go right to the start of the buffer.

If a `beginning-of-buffer' error is thrown, `point' is moved to
`point-min'."
  (condition-case nil
      ad-do-it
    (beginning-of-buffer
     (goto-char (point-min)))))

(defadvice scroll-up (around full-scroll-up activate)
  "Have `scroll-up' go right to the end of the buffer.

If a `end-of-buffer' error is thrown, `point' is moved to
`point-max'."
  (condition-case nil
      ad-do-it
    (end-of-buffer
     (goto-char (point-max)))))

(provide 'fscroll)

;;; fscroll.el ends here
