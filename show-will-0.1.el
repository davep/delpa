;;; show-will.el --- Make it show I can show Will stuff  -*- lexical-binding: t -*-
;; Copyright 2023 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: convenience
;; URL: https://github.com/davep/show-will.el
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
;; show-will.el provides a couple of wrapper commands for `text-scale-set'
;; so I can quickly and easily set my screen up so Will can read my code
;; over my shoulder.

;;; Code:

(defun show-will-on ()
  "Set up a buffer so Will can read it."
  (interactive)
  (text-scale-set 5))

(defun show-will-off ()
  "Set up a buffer I can read it."
  (interactive)
  (text-scale-set 0))

(provide 'show-will)

;;; show-will.el ends here
