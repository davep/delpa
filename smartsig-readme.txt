;;; Commentary:
;;
;; This code is designed for use when editing email and news articles. It
;; lets you create signatures that automatically adapt to what you're
;; writing based on words contained in the body of the text.
;;
;; smartsig allows you to do this by letting you define signatures with
;; associated keywords. The definition is done using `smartsig-add':
;;
;;   (smartsig-add ID SIGNATURE &rest KEYWORDS)
;;
;; ID is a unique ID for the signature, make it something short and
;; meaningful. SIGNATURE is a pointer to the signature file that you want
;; used when this signature becomes applicable. KEYWORDS is an open ended
;; list of keywords that will help trigger the selection of this signature.
;;
;; Because there are a number of mail/news editing modes out there some
;; parts of smartsig need to be configured before it will work. The
;; configuration items are:
;;
;;   `smartsig-start-of-body-function' : This variable should be set to
;;   point to a function that will return the `point' of the start of the
;;   body of the article (IOW the first bit of text after any headers).
;;
;;   `smartsig-end-of-body-function' : This variable should be set to point
;;   to a function that will return the `point' of the end of the body of
;;   the article (IOW the `point' before the signature).
;;
;;   `smartsig-set-signature' : This variable should be set to point to a
;;   function that will set the signature. The function should take a single
;;   parameter that is the name of the signature file.
;;
;;   `smartsig-abbrev-table' : This variable should be set to a symbol that
;;   is the name of the abbrev table you intend to use.
;;
;; Please note that smartsig needs abbrev to work. Once you've configured
;; smartsig to work with your environment you'll still need to turn on
;; `abbrev-mode' for your mail/news editing mode before anything will
;; happen.
;;
;; The latest smartsig.el is always available from:
;;
;;   <URL:https://github.com/davep/smartsig.el>

;;; INSTALLATION:
;;
;; o Drop smartsig.el somewhere into your `load-path'. Try your site-lisp
;;   directory for example (you might also want to byte-compile the file).
;;
;; o Because smartsig needs to be used with your editing mode of choice you
;;   need to create a hook for that mode. The hook would look something like:
;;
;;   (add-hook 'some-mail-editing-mode-hook
;;             (lambda ()
;;               (require 'smartsig)
;;               (setq smartsig-set-signature #'function-for-setting-signature)
;;               (setq smartsig-abbrev-table  'some-mail-editing-mode-abbrev-table)
;;               (smartsig-clear)
;;               (smartsig-add "emacs" "~/.sigs/emacs" "emacs" "xemacs" "elisp" "gnu")
;;               (smartsig-add "mutt"  "~/.sigs/mutt"  "mutt" "mail" "email" "elkins")
;;               (abbrev-mode 1)))
;;
;;   where `some-mail-editing-mode-hook' is the hook for the editing mode
;;   you use and `some-mail-editing-mode-abbrev-table' is the abbrev table
;;   you use in that editing mode.

