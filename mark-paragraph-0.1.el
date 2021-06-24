;;; mark-paragraph.el --- Commands for marking a paragraph -*- lexical-binding: t -*-
;; Copyright 2021 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.01
;; Keywords: convenience
;; URL: https://github.com/davep/mark-paragraph.el
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
;; mark-paragraph.el contains some extra commands for marking a paragraph.
;; Inspired by https://twitter.com/nriley/status/1408049507775004675, I
;; checked Emacs and actually couldn't see a simple way of doing it out of
;; the box, so I quickly knocked this up for fun.
;;
;; Note that, right now, this doesn't attempt to do anything clever; I just
;; knocked this up as a reminder/placeholder to look and see if there's a
;; better way of doing this (it is something I've often found myself wanting
;; to have to hand).

;;; Code:

(defun mark-paragraph-to-start ()
  "Mark from `point' to the start of the paragraph.

`start-of-paragraph-text' is used to find the start of the
paragraph."
  (interactive)
  (save-excursion
    (start-of-paragraph-text)
    (push-mark-command prefix-arg)))

(defun mark-paragraph-to-end ()
  "Mark from `point' to the end of the paragraph.

`end-of-paragraph-text' is used to find the end of the
paragraph."
  (interactive)
  (save-excursion
    (end-of-paragraph-text)
    (push-mark-command prefix-arg)))

(provide 'mark-paragraph)

;;; mark-paragraph.el ends here
