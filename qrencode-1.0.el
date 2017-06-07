;;; qrencode.el --- QR code text with qrenco.de -*- lexical-binding: t -*-
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/qrencode.el
;; Package-Requires: ((emacs "24"))

;; qrencode.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; qrencode.el provides a simple Emacs interface for turning text into
;; text-based QR codes, via qrenco.de. Originally written as a bit of a
;; lunchtime hack after seeing:
;;
;; https://twitter.com/asciinema/status/872410405976584192
;;
;; on twitter.

;;; Code:

(defconst qrencode-url "http://qrenco.de/%s"
  "URL for qrenco.de.")

(defconst qrencode-user-agent "qrencode.el (curl)"
  "User agent to send to qrenco.de.

Note that \"curl\" should ideally be included in the user agent
string because of the way qrenco.de works.

qrenco.de looks for a specific set of clients in the user
agent (see https://goo.gl/amnfAW for this) to decide if it should
deliver plain text rather than HTML. qrencode.el requires plain
text.")

(defun qrencode-tidy-text (text)
  "Tidy up TEXT for sending to qrenco.de."
  (replace-regexp-in-string "\n" "\\n" text t t))

(defun qrencode-get-qr (text)
  "Get a QR code from qrenco.de for TEXT."
  (let ((text (qrencode-tidy-text text)))
    (with-current-buffer
        (let ((url-request-extra-headers `(("User-Agent" . ,qrencode-user-agent))))
          (url-retrieve-synchronously (format qrencode-url (url-hexify-string text)) t t))
      (set-buffer-multibyte t)
      (setf (point) (point-min))
      (when (search-forward-regexp "^$" nil t)
        (buffer-substring (point) (point-max))))))

;;;###autoload
(defun qrencode (text)
  "Show a QR code for TEXT in a window."
  (interactive "sText: ")
  (let ((qr (qrencode-get-qr text)))
    (if current-prefix-arg
        (insert qr)
      (with-help-window "*QR Code"
        (princ qr)))))

;;;###autoload
(defun qrencode-region (start end)
  "Show text between START and END as a QR code."
  (interactive "r")
  (deactivate-mark)
  (qrencode (buffer-substring-no-properties start end)))

;;;###autoload
(defun qrencode-maybe-region ()
  "If a region is active, QR encode that, otherwise prompt."
  (interactive)
  (call-interactively (if mark-active #'qrencode-region #'qrencode)))

(provide 'qrencode)

;;; qrencode.el ends here
