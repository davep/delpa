;;; Commentary:
;;
;; This package provides a simple method of popping up a list of URLs and
;; allowing the selection and insertion of an URL into the previous buffer.
;;
;; The URLs are stored in an external file as a list of cons cells, for example:
;;
;; (( "The GNU Project" . "http://www.gnu.org/")
;;  ( "The FSF"         . "http://www.fsf.org/"))
;;
;; The name and location of the file is up to you, the default name used by
;; the function `handyurl' is stored in `handyurl-file'.
;;
;; Note that this code then went on to inspire quickurl.el, which then went
;; on to be part of GNU Emacs itself.

