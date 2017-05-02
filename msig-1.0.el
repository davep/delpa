;;; msig.el --- Tools for manipulating email/news signatures
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: mail
;; URL: https://github.com/davep/msig.el

;; msig.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2 or (at your option) any later version. For
;; details see the file COPYING.

;;; Commentary:
;;
;; msig.el provides some simple commands that allow for the easy
;; manipulation of the email/news signature in a buffer.

;;; Code:

(defvar msig-sep "-- \n"
  "Signature seperator string.")

(defvar msig-sep-regexp "^-- $"
  "Regular expression for finding the signature seperator line.")

;;;###autoload
(defun msig-kill ()
  "Kill the signature from the buffer.

Returns the `point' for where the signature was or, if there
isn't a signature, the `point' of the end of the buffer."
  (interactive)
  (save-excursion
    (setf (point) (point-min))
    (cond ((search-forward-regexp msig-sep-regexp nil t)
	   (beginning-of-line)
	   (kill-region (point) (point-max)))
	  (t
           (setf (point) (point-max))))
    (point)))

;;;###autoload
(defun msig-set (file)
  "Set the signature in the current buffer to FILE.

If FILE is an executable the signature will be set to the output
of FILE."
  (interactive "fSignature: ")
  (save-excursion
    (setf (point) (msig-kill))
    (insert msig-sep)
    (if (file-executable-p file)
        (insert (shell-command-to-string file))
      (insert-file-contents file))))

(provide 'msig)

;;; msig.el ends here
