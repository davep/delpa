;;; chrurl.el --- Insert the URL of the 'topmost' Chrome -*- lexical-binding: t -*-
;; Copyright 2020 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: hypermedia
;; URL: https://github.com/davep/chrurl.el
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
;; chrurl.el provides a simple command that will insert the URL of the
;; "topmost" instance of Google Chrome into the current buffer.
;;
;; Note that, at the moment, this only works on macOS. I seldom work for any
;; sensible time in any other environment so adding other platforms isn't a
;; priority right at the moment.

;;; Code:

(require 'subr-x)

(defvar chrurl-macos "
if application \"Google Chrome\" is running then
  tell app \"Google Chrome\" to get URL of active tab of first window
end if
"
  "AppleScript code to get the URL from Chrome.")

;;;###autoload
(defun chrurl ()
  "Insert the URL of the 'topmost' instance of Chrome."
  (interactive)
  (if-let ((url (string-trim (shell-command-to-string (format "osascript -e '%s'" chrurl-macos)))))
      (insert url)
    (error "Could not get the URL from Google Chrome")))

(provide 'chrurl)

;;; chrurl.el ends here
