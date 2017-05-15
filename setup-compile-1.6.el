;;; setup-compile.el --- Command for setting up a compile command.
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.6
;; Keywords: convenience
;; URL: https://github.com/davep/setup-compile.el

;; setup-compile.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; setup-compile.el provides a command that makes it easy to quickly set a
;; sensible compile command for the given buffer. Essentially it checks to
;; see if there's a Makefile in the directory and, if there isn't, it builds
;; a command that should do the job.

;;; Code:

(defgroup setup-compile nil
  "Command for setting up a compile command."
  :group  'convenience
  :prefix "setup-compile-")

(defcustom setup-compile-default-commands
  (list
   (cons 'c-mode          "gcc -Wall -O2 {{src}} -o {{exe}}")
   (cons 'c++-mode        "g++ -Wall -O2 {{src}} -o {{exe}}")
   (cons 'emacs-lisp-mode "emacs -batch -f batch-byte-compile {{src}}"))
  "List of default commands based on major mode names."
  :type '(repeat
          (cons :tag "Compile command"
                (symbol :tag "Major mode")
                (string :tag "Compile command")))
  :group 'setup-compile)

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
  (when buffer-file-name
    (or (file-exists-p "GNUmakefile")
        (file-exists-p "makefile")
        (file-exists-p "Makefile")
        (let ((cmd (or (cdr (assoc major-mode setup-compile-default-commands)) default-command)))
          (when cmd
            (set (make-local-variable 'compile-command)
                 (replace-regexp-in-string
                  "{{exe}}" (file-name-sans-extension (file-name-nondirectory buffer-file-name))
                  (replace-regexp-in-string
                   "{{src}}" buffer-file-name cmd))))))))

(provide 'setup-compile)

;;; setup-compile.el ends here
