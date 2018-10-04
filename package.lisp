(eval-when (:load-toplevel :compile-toplevel :execute)
  (ql:quickload '("hunchentoot" "cl-who" "smackjack" "parenscript")))

(defpackage #:web-net
  (:use :hunchentoot :cl-who :smackjack :parenscript))
