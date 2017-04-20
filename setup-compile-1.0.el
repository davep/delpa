;;; setup-compile.el --- Command for setting up a compile command.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.0
;; Keywords: convenience
;; URL: https://github.com/davep/setup-compile.el

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

;;; Code:

;;;###autoload
(defun setup-compile (default-command)
  "Setup the compile command for a buffer.

DEFAULT-COMMAND is the command to use if no other sensible
command can be found."
  (interactive "sDefault compile command: \n")
  (or (file-exists-p "GNUmakefile")
      (file-exists-p "makefile")
      (file-exists-p "Makefile")
      (progn
        (make-local-variable 'compile-command)
        (setq compile-command
              (concat default-command " " buffer-file-name
                      " -o " (file-name-sans-extension
                              (file-name-nondirectory (buffer-file-name))))))))

(provide 'setup-compile)

;;; setup-compile.el ends here
