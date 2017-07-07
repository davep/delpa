;;; Commentary:
;;
;; `url-retrieve-synchronously' has behaved oddly for me from time to time,
;; having `point' settle in different places in the resulting buffer
;; depending on the site I'm pulling content from. This code has been
;; written so I can easily see where `point' ends up depending on what I
;; request, and how.
;;
;; This code really isn't useful for much else, and is unlikely to be useful
;; to anyone else. It's just a handy little debug/learning tool.

