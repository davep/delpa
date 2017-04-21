;;; longmacs.el --- Make it a little harder to deny emacs is an OS
;; Copyright 2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 1.1
;; Keywords: convenience, server
;; URL: https://github.com/davep/longmacs.el
;; Package-Requires: ((bind-key "1.0"))

;; longmacs.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2 or (at your option) any later version.
;; For details see the file COPYING.

;;; Commentary:
;;
;; longmacs.el is designed to help me treat Emacs as something that should
;; be kept running for a long time. There's no need to start and stop an
;; editor like Emacs and this little package contains the bits I use to help
;; me keep bad habits under control.
;;
;; On my iMac and Macbook I tend to start an Emacs sesssion, run
;; `longmacs-enable', turn it into a full-screen application, and then do
;; all my editing in there.
;;
;; See also uptimes.el for another tool to help encourage good Emacs use.

;;; Code:

(require 'bind-key)

;;;###autoload
(defun longmacs-no-exit ()
  "Do nothing other than say we're doing nothing."
  (interactive)
  (message "C-x C-c is disabled"))

;;;###autoload
(defun longmacs-enable ()
  "Turn this emacs sesssion into a long term emacs.

This involves disabling C-x C-c and also calling `server-start'.

If you do need to exit emacs, simply M-x `kill-emacs' RET
instead."
  (interactive)
  (server-start)
  (bind-key "C-x C-c" #'longmacs-no-exit))

(provide 'longmacs)

;;; longmacs.el ends here
