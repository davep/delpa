;;; ngn.el --- Quickly insert a newsgroup name.
;; Copyright 2000-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.6
;; Keywords: convenience, news, gnus, newsrc
;; URL: https://github.com/davep/ngn.el
;; Package-Requires: ((cl-lib "0.5"))

;; ngn.el is free software distributed under the terms of the GNU General
;; Public Licence, version 2 or (at your option) any later version. For
;; details see the file COPYING.

;;; Commentary:
;;
;; ngn.el provides commands for quickly inserting a newsgroup name into a
;; buffer, using complention, building the list from your newsgroup name
;; sources.
;;
;; The latest ngn.el is always available from:
;;
;;   <URL:https://github.com/davep/ngn.el>

;;; TODO:
;;
;; o Do away with `ngn-newsrc' and `ngn-merge-with-gnus-newsrc' and,
;;   instead, provide a flexible method of defining multiple sources for group
;;   names.

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl-lib))

;; Customize options.

(defgroup ngn nil
  "Newgroup name lookup and recall."
  :group  'convenience
  :prefix "ngn-")

(defcustom ngn-newsrc "~/.newsrc"
  "Name of your newsrc file."
  :type  'file
  :group 'ngn)

(defcustom ngn-cache-p t
  "Should we cache the newsgroup names?"
  :type  'boolean
  :group 'ngn)

(defcustom ngn-must-exist t
  "Must the input group name be a known group?"
  :type  'boolean
  :group 'ngn)

(defcustom ngn-url-format-function #'(lambda (group)
                                       (format "<URL:news:%s>" group))
  "Function to format a newsgroup name as an URL."
  :type  'function
  :group 'ngn)

;; Non customising variables.

(defvar ngn-history nil
  "History list for `ngn-reader'.")

(defvar ngn-cache nil
  "Newsgroup name cache.")

;; Main code:

(defun ngn-merge-with-gnus-newsrc (groups)
  "Merge the existing group list GROUPS with the Gnus group list (if available)."
  (if (boundp 'gnus-newsrc-alist)
      (append (cl-loop for group in (mapcar #'car (symbol-value 'gnus-newsrc-alist))
                 unless (assoc group groups)
                 collect (list group))
              groups)
    groups))

(defun ngn-load-group-names ()
  "Load the newsgroup list for use with `completing-read'."
  (if (and ngn-cache ngn-cache-p)
      ngn-cache
    (setq ngn-cache
          (ngn-merge-with-gnus-newsrc
           (when (file-readable-p ngn-newsrc)
             (with-temp-buffer
               (save-excursion
                 (insert-file-contents-literally ngn-newsrc)
                 (while (re-search-forward "[:!].*$" nil t)
                   (replace-match "")))
               (cl-loop until (eobp)
                  collect (list (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                  do (forward-line))))))))

(defun ngn-reader ()
  "Prompt the user for a newsgroup name, using completion."
  (let ((default (car ngn-history)))
    (completing-read (format "Newsgroup%s: " (if default
                                                 (format " (default %s)" default)
                                               ""))
                     (ngn-load-group-names)
                     nil
                     ngn-must-exist
                     ""
                     'ngn-history
                     default)))

;;;###autoload
(defun ngn-clear-cache ()
  "Clear the newsgroup name cache."
  (interactive)
  (setq ngn-cache nil))

;;;###autoload
(defun ngn-insert-name (groupname)
  "Insert GROUPNAME into the current buffer."
  (interactive (list (ngn-reader)))
  (insert groupname))

;;;###autoload
(defun ngn-insert-url (groupname)
  "Insert GROUPNAME, as an URL, into the current buffer."
  (interactive (list (ngn-reader)))
  (insert (funcall ngn-url-format-function groupname)))

;;;###autoload
(defun ngn-insert (no-format)
  "Insert a newsgroup name, formatting the name depending on the prefix.

If NO-FORMAT is 't' (or the command is invoked with
\\[universal-argument]) then `ngn-insert-url' is used to insert
the group name. If a prefix is provided then `ngn-insert-name' is
used."
  (interactive "P")
  (call-interactively (if no-format #'ngn-insert-name #'ngn-insert-url)))

(provide 'ngn)

;;; ngn.el ends here
