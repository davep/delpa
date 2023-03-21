;;; reframe.el --- Size/position frames depending where I am -*- lexical-binding: t -*-
;; Copyright 2023 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
;; Keywords: convenience, frames
;; URL: https://github.com/davep/reframe.el
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
;;
;;; Commentary:
;;
;; reframe.el is a personal helper package that lets me quickly and easily
;; position a frame depending on context.

;;; Code:

(require 'is-a)

(defconst reframe-rules
  '(((lambda ()
       (and is-a-macOS-window-p (string= (downcase (system-name)) "lucien.local"))) . (100 40 300 110))
    ((lambda ()
       (and is-a-macOS-window-p (string= (downcase (system-name)) "raven.local"))) . (2590 50 300 95))
    ((lambda ()
       (and is-a-macOS-window-p (string= (downcase (system-name)) "shadow.local"))) . (2590 50 300 95)))
  "List of rules for reframing.")

;;;###autoload
(defun reframe ()
  "Move and resize the current frame depending on context."
  (interactive)
  (mapcar (lambda (rule)
            (when (funcall (car rule))
              (set-frame-position (selected-frame) (cadr rule) (caddr rule))
              (set-frame-size (selected-frame) (cadddr rule) (car (cddddr rule)))))
          reframe-rules))

(provide 'reframe)

;;; reframe.el ends here
