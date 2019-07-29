;;; gitweb.el --- Visit the web page for this git repo -*- lexical-binding: t -*-
;; Copyright 2019 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: hypermedia
;; URL: https://github.com/davep/gitweb.el
;; Package-Requires: ((emacs "24.4"))

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
;; gitweb.el provides a simple function that will open the project web page
;; for the current git repository, if we're visiting a file within one and
;; if there is an origin available for it.

;;; Code:

(require 'subr-x)

;;;###autoload
(defun gitweb ()
  "Visit the website associated with this repo, if there is one.

The idea here is that, if this repository has a remote GitHub or
GitLab, etc, it'll open the project page for the repo in the web
browser."
  (interactive)
  (let ((origin (string-trim (shell-command-to-string "git config --get remote.origin.url"))))
    (if (string-empty-p origin)
        (error "This doesn't appear to be a git repo with an origin")
      (let ((origin (cadr (split-string origin "@"))))
        (browse-url
         (format "https://%s"
                 (file-name-sans-extension
                  (replace-regexp-in-string ":" "/" origin))))))))

(provide 'gitweb)

;;; gitweb.el ends here
