;;; org-davep.el --- My own little additions to org-mode
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/org-davep.el

;;; Commentary:

;; org-davep.el provides personal utility functions for working with
;; org-mode.

;;; Code:

(require 'org)

;;;###autoload
(defun org-davep-open-dir ()
  "Open the main org directory."
  (interactive)
  (find-file org-directory))

;;;###autoload
(defun org-davep-open-inbox ()
  "Open my main org inbox file."
  (interactive)
  (find-file (concat org-directory "/inbox.org")))

(provide 'org-davep)

;;; org-davep.el ends here
