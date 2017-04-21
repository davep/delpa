;;; setup-compile.el --- Command for setting up a compile command.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: convenience
;; URL: https://github.com/davep/setup-compile.el

;;; Commentary:
;;
;; setup-compile.el provides a command that makes it easy to quickly set a
;; sensible compile command for the given buffer. Essentially it checks to
;; see if there's a Makefile in the directory and, if there isn't, it builds
;; a command that should do the job.

;;; Code:

(defvar setup-compile-default-commands
  '((c-mode   . "gcc -Wall -O2 {{src}} -o {{exe}}")
    (c++-mode . "g++ -Wall -O2 {{src}} -o {{exe}}"))
  "List of default commands based on major mode names.")

;;;###autoload
(defun setup-compile (&optional default-command)
  "Setup the compile command for a buffer.

DEFAULT-COMMAND is the command to use if no other sensible
command can be found.

Default commands can be found in `setup-compile-default-commands'.

Defaults commands, either from DEFAULT-COMMAND or from
`setup-compile-default-commands', can contain the following replaceable tokens:

  {{src}} - The full path to the source file.
  {{exe}} - The possible resulting executable name (this is the source
            file with the path and extension removed)."
  (interactive "sDefault compile command: \n")
  (or (file-exists-p "GNUmakefile")
      (file-exists-p "makefile")
      (file-exists-p "Makefile")
      (let ((cmd (or (cdr (assoc 'c-mode setup-compile-default-commands)) default-command)))
        (when cmd
          (set (make-local-variable 'compile-command)
               (replace-regexp-in-string
                "{{exe}}" (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                (replace-regexp-in-string
                 "{{src}}" buffer-file-name cmd)))))))

(provide 'setup-compile)

;;; setup-compile.el ends here
