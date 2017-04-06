;;; icmp-info.el --- ICMP information tool.
;; Copyright 2001-2017 by Dave Pearson <davep@davep.org>

;; Author: Dave Pearson <davep@davep.org>
;; Version: 2.1
;; Keywords: convenience, quoting
;; URL: https://github.com/davep/icmp-info.el
;; Package-Requires: ((cl-lib "0.5"))

;; icmp-info.el is free software distributed under the terms of the GNU
;; General Public Licence, version 2. For details see the file COPYING.

;;; Commentary:
;;
;; icmp-info.el provides a set of functions for getting information about
;; ICMP types and codes. This could be useful for the occasional packet log
;; reader.
;;
;; The latest icmp-info.el is always available from:
;;
;;   <URL:https://github.com/davep/icmp-info.el>

;;; Code:

;; Things we need:

(eval-when-compile
  (require 'cl-lib))

;; Constants.

(defconst icmp-info-types '((echo-reply                  "Echo reply (pong)")
                            nil                          ; Unassigned
                            nil                          ; Unassigned
                            (destination-unreachable     "Destination Unreachable"
                             (network-unreachable        "Network Unreachable")
                             (host-unreachable           "Host Unreachable")
                             (protocol-unreachable       "Protocol Unreachable")
                             (port-unreachable           "Port Unreachable")
                             (fragmentation-needed       "Fragmentation Needed")
                             (source-route-failed        "Source Route Failed")
                             (network-unknown            "Network Unknown")
                             (host-unknown               "Host Unknown")
                             (network-prohibited         "Network Prohibited")
                             (source-host-isolated       "Source Host Isolated")
                             (host-prohibited            "Host Prohibited")
                             (TOS-network-unreachable    "TOS Network Unreachable")
                             (TOS-host-unreachable       "TOS Host Unreachable")
                             (communication-prohibited   "Communication Prohibited")
                             (host-precedence-violation  "Host Precedence Violation")
                             (precedence-cutoff          "Precedence Cutoff"))
                            (source-quench               "Source Quench")
                            (redirect                    "Redirect"
                             (network-redirect           "Network Redirect")
                             (host-redirect              "Host Redirect")
                             (TOS-network-redirect       "TOS Network Redirect")
                             (TOS-host-redirect          "TOS Host Redirect"))
                            (alternate-host-address      "Alternate Host Address")
                            nil                          ; Unassigned
                            (echo-request                "Echo Request (ping)")
                            (router-advertisement        "Router Advertisement")
                            (router-solicitation         "Router Solicitation")
                            (time-exceeded               "Time Exceeded (TTL exceeded)"
                             (ttl-zero-during-transit    "TTL Zero During Transit")
                             (ttl-zero-during-reassembly "TTL Zero During Reassembly"))
                            (parameter-problem           "Parameter Problem"
                             (ip-header-bad              "IP Header Bad")
                             (required-option-missing    "Required Option Missing")
                             (bad-length                 "Bad Length"))
                            (timestamp-request           "Timestamp Request")
                            (timestamp-reply             "Timestamp Reply")
                            (information-request         "Information Request")
                            (information-reply           "Information Reply")
                            (address-mask-request        "Address Mask Request")
                            (address-mask-reply          "Address Mask Reply")
                            nil                          ; Reserved (for Security)
                            nil nil nil nil nil          ; Reserved (for Robustness Experiment)
                            nil nil nil nil nil          ; Reserved (for Robustness Experiment)
                            (traceroute                  "Traceroute")
                            (datagram-conversion-error   "Datagram Conversion Error")
                            (mobile-host-redirect        "Mobile Host Redirect")
                            (ipv6-where-are-you          "IPv6 Where Are You")
                            (ipv6-i-am-here              "IPv6 I Am Here")
                            (mobile-registration-request "Mobile Registration Request")
                            (mobile-registration-reply   "Mobile Registration Reply")
                            (domain-name-request         "Domain Name Request")
                            (domain-name-reply           "Domain Name Reply")
                            (skip                        "SKIP")
                            (photuris                    "Photuris"))
  "List of ICMP types and codes.")

;; Main code.

(defun icmp-info-type (result)
  "Return the type code of a the given ICMP RESULT."
  (if (consp (car result))
      (caar result)
    (car result)))

(defun icmp-info-code (result)
  "Return the code number of an ICMP result.

Note that if no code is associated with RESULT the return value will be 0."
  (if (consp (car result))
      (cdar result)
    0))

(defun icmp-info-symbol (result)
  "Return the symbol name of the ICMP type/code in RESULT."
  (cadr result))

(defun icmp-info-name (result)
  "Return the name of the ICMP type/code in RESULT."
  (nth 2 result))

(defun icmp-info-type-p (result)
  "Is RESULT an ICMP type?"
  (integerp (car result)))

(defun icmp-info-code-p (result)
  "Is RESULT an ICMP code?"
  (not (icmp-info-type-p result)))

(defun icmp-info-codes (result)
  "Return a list of ICMP codes associated with the type of RESULT."
  (cl-loop for info in (nth 3 result)
     for code = 0 then (1+ code)
     collect (icmp-info-make-result (icmp-info-type result) code info)))

(defun icmp-info-make-result (type code details)
  "Make an ICMP result for TYPE, CODE and DETAILS."
  (list (if code (cons type code) type)
        (car details)
        (cadr details)
        (cddr details)))

(defun icmp-info-lookup-type (type &optional code)
  "Lookup ICMP TYPE or (optionally) CODE."
  (let ((icmp (nth type icmp-info-types)))
    (when icmp
      (cond ((not code)
             (icmp-info-make-result type code icmp))
            ((and (null (cddr icmp)) (zerop code))
             (icmp-info-make-result type nil icmp))
            ((and code (<= code (length (cddr icmp))))
             (icmp-info-make-result type code (nth code (cddr icmp))))
            (t
             nil)))))

(defun icmp-info-lookup-symbol-1 (symbol list &optional parent)
  "Look for SYMBOL in LIST."
  (cl-loop for item in list
     for count = 0 then (1+ count)
     if (and item (eq (car item) symbol))
     return (icmp-info-make-result
             (if parent parent count)
             (when parent count)
             item)
     else if (and item (cddr item) (icmp-info-lookup-symbol-1 symbol (cddr item) count))
     return (icmp-info-lookup-symbol-1 symbol (cddr item) count)))

(defun icmp-info-lookup-symbol (symbol)
  "Find ICMP details by looking for SYMBOL."
  (icmp-info-lookup-symbol-1 symbol icmp-info-types))

(defun icmp-info-symbols ()
  "Return an assoc list of ICMP symbols.

The resulting list is designed for use with `completing-read'."
  (let ((symbols (list)))
    (cl-loop for type in icmp-info-types
       when type do (push (list (symbol-name (car type))) symbols)
       when (cddr type)
       do (cl-loop for code in (cddr type)
             do (push (list (symbol-name (car code))) symbols)))
    (nreverse symbols)))

;;;###autoload
(defun icmp-info-lookup (find1 find2)
  "Interactively find details about an ICMP type or code."
  (interactive (let* ((find1 (completing-read "Type: " (icmp-info-symbols) nil nil "" nil))
                      (find2 (when (integerp (car (condition-case nil (read-from-string find1) (error nil))))
                               (read-from-minibuffer "Code: "))))
                 (list (car (condition-case nil (read-from-string find1) (error nil)))
                       (car (condition-case nil (read-from-string find2) (error nil))))))
  (let ((result (cond ((and (integerp find1) (or (null find2) (integerp find2)))
                       (icmp-info-lookup-type find1 find2))
                      ((symbolp find1)
                       (icmp-info-lookup-symbol find1)))))
    (if result
        (let* ((codes (icmp-info-codes result))
               (count (length codes)))
          (message "%s, type %d%s%s%s"
                   (icmp-info-name result)
                   (icmp-info-type result)
                   (if (and codes (null find2)) "" ", code ")
                   (if (and codes (null find2)) "" (format "%d" (icmp-info-code result)))
                   (if codes
                       (format ", %d code%s %s associated with this type"
                               count
                               (if (= count 1) "" "s")
                               (if (= count 1) "is" "are"))
                     "")))
      (error "ICMP lookup parameters make no sense"))))

;;;###autoload
(defun icmp-info-list ()
  "Create a buffer that lists all known ICMP types and codes."
  (interactive)
  (with-output-to-temp-buffer "*icmp list*"
    (princ "Type  Code  Name\n----  ----  ----------------------------------------\n")
    (cl-loop for type in icmp-info-types
       for type-code = 0 then (1+ type-code)
       when type do (princ (format "%4d        %s\n" type-code (cadr type)))
       when (cddr type) do (cl-loop for code in (cddr type)
                              for code-num = 0 then (1+ code-num)
                              do (princ (format "      %4d  %s\n" code-num (cadr code))))
       when type do (princ "\n"))))

(provide 'icmp-info)

;;; icmp-info.el ends here
