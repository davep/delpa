;;; nuke-buffers.el --- Safely kill as many buffers as possible
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/nuke-buffers.el
;; Package-Requires: ((dash "2.12"))

;; nuke-buffers.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; nuke-buffers.el provides a command that aims to kill as many buffers as
;; possible without causing any work to be lost or any important buffers to
;; be lost.
;;
;; CAUTION: This works based on my own idea of what is "safe"!

;;; Code:

(require 'dash)

(defgroup nuke-buffers nil
  "Safely kill as many buffers as possible."
  :group  'convenience
  :prefix "nuke-buffers-")

(defcustom nuke-buffers-ignore
  (list
   "*scratch*"
   "*Messages*")
  "List of buffers we should always ignore."
  :type '(repeat (string :tag "Buffer name: ")))

(defun nuke-buffers-ignore-p (buffer)
  "Is BUFFER ignorable?"
  (let ((name (buffer-name buffer)))
    (or
     ;; Minibuffer?
     (minibufferp buffer)
     ;; "Special" buffer?
     (string-match "^ \\*" name)
     ;; A buffer on the ignore list?
     (member name nuke-buffers-ignore))))

(defun nuke-buffers-unsaved-file-buffer-p (buffer)
  "Is BUFFER visiting an unsaved file?"
  (and (buffer-file-name buffer) (buffer-modified-p buffer)))

(defun nuke-buffers-get-candidates ()
  "Get the list of buffers we're likely to nuke."
  (-filter #'(lambda (buffer)
               (and
                ;; If it's not a minibuffer...
                (not (minibufferp buffer))
                ;; ...and it's not a buffer we should always ignore...
                (not (nuke-buffers-ignore-p buffer))
                ;; ...and it's not an unsaved file...
                (not (nuke-buffers-unsaved-file-buffer-p buffer))
                ;; ..and it's not the current buffer.
                (not (eq buffer (current-buffer)))))
           (buffer-list)))

;;;###autoload
(defun nuke-buffers ()
  "Kill as many buffers as possible, without losing important things."
  (interactive)
  (mapc #'kill-buffer (nuke-buffers-get-candidates)))

(provide 'nuke-buffers)

;;; nuke-buffers.el ends here
