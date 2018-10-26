(eval-when (:load-toplevel :execute :compile-toplevel)
  (ql:quickload '("hunchentoot" "cl-who" "smackjack")))

(defpackage :net-vis
  (:use :hunchentoot :cl-who :cl-json :cl :smackjack)
  (:export :start-server :stop-server :make-node :make-link :make-json-graph :*server* :send-node-data :send-search-results
           :make-auto-complete-tree :make-autocomplete-tree-from-map :*auto-complete-data*))
