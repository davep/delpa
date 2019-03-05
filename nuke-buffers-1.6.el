;;; nuke-buffers.el --- Safely kill as many buffers as possible
;; Copyright 2017-2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.6
;; Keywords: convenience
;; URL: https://github.com/davep/nuke-buffers.el
;; Package-Requires: ((emacs "25.1"))

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
;; nuke-buffers.el provides a command that aims to kill as many buffers as
;; possible without causing any work to be lost or any important buffers to
;; be lost.
;;
;; CAUTION: This works based on my own idea of what is "safe"!

;;; Code:

(require 'seq)
(require 'dired)

(defgroup nuke-buffers nil
  "Safely kill as many buffers as possible."
  :group  'convenience
  :prefix "nuke-buffers-")

(defcustom nuke-buffers-ignore
  (list
   "*scratch*"
   "*Messages*")
  "List of buffers we should always ignore."
  :type '(repeat (string :tag "Buffer name: "))
  :group 'nuke-buffers)

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
  (seq-filter (lambda (buffer)
                (and
                 ;; If it's not a buffer we should always ignore...
                 (not (nuke-buffers-ignore-p buffer))
                 ;; ...and it's not an unsaved file...
                 (not (nuke-buffers-unsaved-file-buffer-p buffer))
                 ;; ..and it's not the current buffer.
                 (not (eq buffer (current-buffer)))))
              (buffer-list)))

(defun nuke-buffers-get-buffer-directory (buffer)
  "Attempt to get the directory that BUFFER is visiting."
  (let ((buffer-dir
         (or
          ;; Is it a buffer that's visiting a file?
          (when (buffer-file-name buffer)
            (file-name-directory (buffer-file-name buffer)))
          ;; If not, perhaps it's a dired buffer?
          (with-current-buffer buffer dired-directory))))
    (when buffer-dir
      (file-truename buffer-dir))))

(defun nuke-buffers-get-candidates-from (directory)
  "Get a list of buffers that can be nuked below DIRECTORY.

This function looks for buffers that are visiting files, and only
files which have been saved. At the moment it *doesn't* return a
buffers visiting an actual directory (so it doesn't return dired
buffers). I'd like to change this at some point."
  (let* ((directory (file-truename directory))
         (directory-len (length directory)))
    (seq-filter
     (lambda (buffer)
       (let* ((buffer-dir (nuke-buffers-get-buffer-directory buffer))
              (buffer-dir-len (length buffer-dir)))
         (when buffer-dir
           (and
            ;; ...and if it relates to a saved file...
            (not (nuke-buffers-unsaved-file-buffer-p buffer))
            ;; ...and if it's within the given directory...
            (string=
             directory
             (substring buffer-dir 0 (min buffer-dir-len directory-len)))))))
     (buffer-list))))

;;;###autoload
(defun nuke-buffers ()
  "Kill as many buffers as possible, without losing important things."
  (interactive)
  (message "Nuked %d buffer(s)."
           (length
            (mapc #'kill-buffer (nuke-buffers-get-candidates)))))

;;;###autoload
(defun nuke-buffers-from-directory (directory)
  "Nuke all buffers relating to files in DIRECTORY.

Note that `nuke-buffers-ignore' is itself ignored here. Unsaved
buffers are, however, bypassed."
  (interactive "DNuke all within: ")
  (message "Nuked %d buffer(s) related to %s."
           (length
            (mapc #'kill-buffer (nuke-buffers-get-candidates-from directory)))
           directory))

(provide 'nuke-buffers)

;;; nuke-buffers.el ends here
