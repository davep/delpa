;;; moving.el --- Extra commands for moving about a buffer.
;; Copyright 2000-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.3
;; Keywords: convenience
;; URL: https://github.com/davep/moving.el

;;; Commentary:
;;
;; moving.el provides some extra commands for moving around a buffer.

;;; Code:

;;;###autoload
(defun moving-to-next-repeated-word ()
  "Find the the next repeated word."
  (interactive)
  (search-forward-regexp "\\(\\<\\w+\\>\\)\\(\\s-\\|\t\\|\n\\)+\\<\\1\\>"))

;;;###autoload
(defun moving-home ()
  "Move `point' towards \"home\" depending on context."
  (interactive)
  (cond ((bolp)
         (setf (point) (point-min)))
        ((<= (point) (save-excursion (back-to-indentation) (point)))
         (beginning-of-line))
        (t
         (back-to-indentation))))

;;;###autoload
(defun moving-end ()
  "Move `point' towards \"end\" depending on context."
  (interactive)
  (cond ((eolp)
         (setf (point) (point-max)))
        ((< (point) (save-excursion (back-to-indentation) (point)))
         (back-to-indentation))
        (t
         (end-of-line))))

;;;###autoload
(defun moving-backward-page (&optional count)
  "Call `backward-page' with COUNT then move to start of line."
  (interactive "p")
  (backward-page count)
  (beginning-of-line))

;;;###autoload
(defun moving-forward-page (&optional count)
  "Call `forward-page' with COUNT them move to start of line."
  (interactive "p")
  (forward-page count)
  (beginning-of-line))

(provide 'moving)

;;; moving.el ends here
