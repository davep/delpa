;;; macinfo.el --- Show information from a Mac's serial number
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 0.1
;; Keywords: convenience
;; URL: https://github.com/davep/macinfo.el
;; Package-Requires: ((cl-lib "0.5"))

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
;; macinfo.el provides functions and commands for getting information out of
;; the serial number of an Apple Mac.
;;
;; The code is currently incomplete and a work in progress.
;;
;; Sources of information used to write this code include:
;; https://www.finetunedmac.com/forums/ubbthreads.php?ubb=showflat&Number=32011
;; http://blog.coriolis.ch/2011/08/01/get-your-apple-device-model-name-in-a-readable-format/

;;; Code:

(require 'url)
(require 'cl-lib)

(defun macinfo-decode-sn-11 (sn)
  "Decode the content of 11 digit serial number SN."
  (error "11 digit serial numbers aren't handled yet"))

(defun macinfo-decode-sn-12-year-decoder (year)
  "Decode YEAR from a serial number into something more useful.

The return value is a `cons' where the `car' is t or nil to show
if the hardware was manufactured early (t) or late (nil) in the
year; the `cdr' is the year."
  (let* ((lookup "123456789CDFGHJKLMNPQRTVWX")
         (match  (cl-search year lookup)))
    (when match
      (cons (zerop (mod match 2)) (+ 2005 (truncate (/ match 2)))))))

(defun macinfo-decode-sn-12-year (year)
  "Decode and describe YEAR from a serial number."
  (let ((decoded (macinfo-decode-sn-12-year-decoder year)))
    (when decoded
      (format "%s %d" (if (car decoded) "Early" "Late") (cdr decoded)))))

(defun macinfo-decode-sn-12-week (week year)
  "Decode the week of manufacture from a serial number.

WEEK is the week code found in the serial number; YEAR is the
year code found in the serial number."
  (let* ((lookup "0123456789CDFGHJKLMNPQRTVWX")
         (match  (cl-search week lookup))
         (year   (macinfo-decode-sn-12-year-decoder year)))
    (when match
      (+ match (if (car year) 1 28)))))

(defun macinfo-decode-sn-12-model-code (mc)
  "Describe model code MC."
  (with-current-buffer
      (url-retrieve-synchronously (format "http://support-sp.apple.com/sp/product?cc=%s&lang=en_US" mc) t)
    (setf (point) (point-min))
    (when (search-forward-regexp "<configCode>\\(.*\\)</configCode>")
      (match-string 1))))

(defun macinfo-decode-sn-12 (sn)
  "Decode the content of 12 digit serial number SN."
  (list
   (cons 'plant-code          (substring sn 0 3))
   (cons 'year-of-manufacture (macinfo-decode-sn-12-year (substring sn 3 4)))
   (cons 'week-of-manufacture (macinfo-decode-sn-12-week (substring sn 4 5) (substring sn 3 4)))
   (cons 'unique-unit-code    (substring sn 5 8))
   (cons 'model-code          (macinfo-decode-sn-12-model-code (substring sn 8 12)))))

(defconst macinfo-decoders
  '((11 . macinfo-decode-sn-11)
    (12 . macinfo-decode-sn-12))
  "Association list of serial number decoders.")

(defun macinfo-get-serial-number ()
  "Get the serial number."
  (with-temp-buffer
    (let ((sysprof "system_profiler"))
      (when (executable-find sysprof)
        (save-excursion
          (call-process sysprof nil t nil "SPHardwareDataType"))
        (when (search-forward "Serial Number (system): " nil t)
          (buffer-substring-no-properties (point) (point-at-eol)))))))

(defun macinfo-sn-funcall-or-error (sn f)
  "If SN is non-nil, apply SN to F, otherwise show an error."
  (if sn
      (funcall f sn)
    (error "Unable to get the serial number, or this isn't a Mac")))

;;;###autoload
(defun macinfo-serial-number ()
  "Show the serial number of a Mac."
  (interactive)
  (macinfo-sn-funcall-or-error
   (macinfo-get-serial-number)
   (lambda (sn)
     (message "Serial number: %s" sn))))

(defun macinfo-show-decoded-serial-number (sn)
  "Decode and show the information in serial number SN."
  (let ((decoder (cdr (assoc (length sn) macinfo-decoders)))
        (help-window-select t))
    (with-help-window "*macinfo*"
      (if decoder
          (cl-flet ((item (lambda (data id) (cdr (assoc id data)))))
            (let ((data (funcall decoder sn)))
              (princ (format "Information found in serial number %s:\n\n" sn))
              (princ (format "Manufacture plant : %s\n" (item data 'plant-code)))
              (princ (format "Manufacture year  : %s\n" (item data 'year-of-manufacture)))
              (princ (format "Manufacture week  : %s\n" (item data 'week-of-manufacture)))
              (princ (format "Model             : %s"   (item data 'model-code)))))
        (princ "Sorry, I don't know how to decode %s" sn)))))

;;;###autoload
(defun macinfo ()
  "Show as much information as we can gather from a Mac's serial number."
  (interactive)
  (macinfo-sn-funcall-or-error
   (macinfo-get-serial-number)
   (lambda (sn)
     (macinfo-show-decoded-serial-number sn))))

(provide 'macinfo)

;;; macinfo.el ends here
