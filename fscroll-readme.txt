;;; Commentary:

;; Out of the box GNU Emacs only scrolls the buffer when using `scroll-down'
;; and `scroll-up' (which most people would have bound to PgDn and PgUp).
;; Once the first or last lines are visible you'll get start/end of buffer
;; errors if you try and hit those keys more. Many other editors will, when
;; that happens, move the cursor to the start or end of the buffer. This
;; code makes GNU Emacs do that too.

