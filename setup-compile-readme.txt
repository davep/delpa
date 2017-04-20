;;; Commentary:
;;
;; setup-compile.el provides a command that makes it easy to quickly set a
;; sensible compile command for the given buffer. Essentially it checks to
;; see if there's a Makefile in the directory and, if there isn't, it builds
;; a command that should do the job.
;;
;; For now it's kind of hard-coded for C/C++ compilation. This could (and
;; should) easily change -- although I've been using this code more or less
;; as-is since the late 1990s.

