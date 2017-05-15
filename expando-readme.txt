;;; Commentary:
;;
;; expando.el provides a simple tool for expanding macros into a different
;; window so they can be quickly and easily checked and read.
;;
;; NOTE: This code makes use of `elisp--preceding-sexp' from elisp-mode.
;; This is, obviously, considered to be an "internal" function. Which is a
;; bit of shame really as it's a very handy thing that I think we'd normally
;; want to be doing in elisp, when doing elisp-oriented things. So, rather
;; than reinvent the wheel, I'll risk calling an internal and worry later
;; when things get broken.

