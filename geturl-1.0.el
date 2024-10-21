;;; geturl.el --- Insert the current URL from a browser of choice -*- lexical-binding: t -*-
;; Copyright 2024 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: hypermedia
;; URL: https://github.com/davep/geturl.el
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
;; geturl.el provides a simple command that will insert the URL of the
;; "topmost" instance of your web browser of choice.
;;
;; Note that, at the moment, this only works on macOS. I seldom work for any
;; sensible time in any other environment so adding other platforms isn't a
;; priority right at the moment. The code is, however, written in a way that
;; it would be simple enough to extend.

;;; Code:

(defgroup geturl nil
  "Get the URL from the top-most browser window."
  :group 'convenience
  :prefix "geturl-")

(defcustom geturl-macos-script "
if application \"Google Chrome\" is running then
  tell application \"Google Chrome\" to get URL of active tab of first window
else if application \"Safari\" is running then
  tell application \"Safari\" to get URL of current tab of first window
end if
"
  "AppleScript code to get the URL from the browser on macOS.

By default it looks to see if Google Chrome is running, then
takes the URL from that. If it isn't it them looks for Safari and
takes it from that if it's running."
  :type 'text
  :group 'geturl)

;;;###autoload
(defun geturl-insert (&optional markup)
  "Insert the URL of the 'topmost' instance of your web browser.

MARKUP is a prefix argument. nil means insert the URL as-is. any
other value means the URL should be inserted wrapped in <URL:...>
markup."
  (interactive "*P")
  (if (eq system-type 'darwin)
      (if-let ((url (string-trim (shell-command-to-string
                                  (format "osascript -e '%s'"
                                          geturl-macos-script)))))
          (insert (if markup (format "<URL:%s>" url ) url))
        (error "Could not get the URL"))
    (error "System type '%s' is currently unsupported" system-type)))

(provide 'geturl)

;;; geturl.el ends here
