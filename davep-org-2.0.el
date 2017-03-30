;;; davep-org.el --- Tools for managing my web sites.
;; Copyright 2001-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 2.0
;; Keywords: convenience
;; URL: https://github.com/davep/davep-org.el

;;; Commentary:
;;
;; davep-org.el provides a handful of tools that help me when I'm working on
;; my personal websites.

;;; Code:

(defvar davep-org-root "~/Sites/www.davep.org/"
  "Root directory of www.davep.org sources.")

(defun davep-org-path (path)
  "Return PATH within `davep-org-root'."
  (format "%s/%s" davep-org-root path))

(defvar davep-org-wxw-root "~/Sites/wxw.davep.org/"
  "Root directory of wxw.davep.org sources.")

(defun davep-org-wxw-path (path)
  "Return PATH within `wxw-davep-org-root'."
  (format "%s/%s" davep-org-wxw-root path))

;;;###autoload
(defun davep-org-changelog ()
  "Add an entry to the www.davep.org ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (davep-org-path "site/ChangeLog/ChangeLog")))))

;;;###autoload
(defun davep-org-wxw-changelog ()
  "Add an entry to the wxw.davep.org ChangeLog."
  (interactive)
  (let ((add-log-time-format 'add-log-iso8601-time-string))
    (with-temp-buffer
      (add-change-log-entry nil (davep-org-wxw-path "site/ChangeLog/ChangeLog")))))

(provide 'davep-org)

;;; davep-org.el ends here
