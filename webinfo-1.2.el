;;; webinfo.el --- Get header information about a web server.
;; Copyright 2002-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.2
;; Keywords: comm, net, http, web
;; URL: https://github.com/davep/webinfo.el

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
;; Provides a simple command for getting header information from a web
;; server at a given host, on a given port.

;;; Code:

;;;###autoload
(defun webinfo (host port)
  "Get some information about HTTP server at HOST on PORT."
  (interactive
   (list
    (read-string "Host: " '("localhost" . 0))
    (read-string "Port: " '("80" . 0))))
  (with-output-to-temp-buffer (format "*webinfo - %s:%s*" host port)
    (let ((s (open-network-stream (format "webinfo-%s-%s" host port) nil host port)))
      (when s
        (unwind-protect
             (progn
               (set-process-filter s (lambda (o p) (princ (delete 13 p))))
               (process-send-string s "HEAD / HTTP/1.0\r\n\r\n")
               (while (eq (process-status s) 'open)
                 (sit-for 0.01)))
          (delete-process s))))))

(provide 'webinfo)

;;; webinfo.el ends here
