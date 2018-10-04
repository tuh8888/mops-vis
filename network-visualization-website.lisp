(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("hunchentoot" "parenscript" "cl-who" "smackjack")))

(defpackage :web-net
  (:use :hunchentoot :cl-who :parenscript :cl :smackjack)
  (:export :start-website :get-node :get-graph :to-sendable-format))

(in-package :web-net)

(setq cl-who:*attribute-quote-char* #\")

(defun start-website (&optional (page-uri "index"))
  (let ((ajax-processor (initialize-ajax)))
    (start (make-instance 'easy-acceptor :port 8080))
    (make-page page-uri ajax-processor)))

(defun initialize-ajax ()
  (let ((ajax-processor (make-instance 'ajax-processor :server-uri "/ajax-process")))

    (defun-ajax request-node (node-name) (ajax-processor :callback-data :response-text)
      (to-sendable-format (get-node node-name)))

    (defun-ajax request-graph () (ajax-processor)
      (to-sendable-format (get-graph)))

    (setq *dispatch-table* (list 'dispatch-easy-handlers
                                 (create-ajax-dispatcher ajax-processor)))

    ajax-processor))

(defun get-node (node-name)
  node-name)

(defun get-graph ()
  "hi")

(defun to-sendable-format (data)
  data)

(defun client-side-methods ()
  (ps
    (defun add-node (node-info)
      ((@ graph nodes push) (create name node-info))
      ((@ console log) node-info))

    (defun add-graph (graph-to-add)
      (setf graph graph-to-add))

    (defun get-node (node-id)
      (chain smackjack (request-node node-id add-node)))

    (defun get-graph ()
      (chain smackjack (request-graph add-graph)))))




(defun make-page (page-uri ajax-processor)
  (push (create-static-file-dispatcher-and-handler
         "/stylesheet.css" "c:/Users/pielk/Drive/code/common-lisp/web-net/stylesheet.css")
        *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/display.js" "c:/Users/pielk/Drive/code/common-lisp/web-net/display.js")
        *dispatch-table*)
  
  (let ((title "Network Visualization"))
   (define-easy-handler (network-display :uri (concatenate 'string "/" page-uri)) ()
     (with-html-output-to-string (s)
       (:html
        (:head
         (:link :rel "stylesheet" :href "stylesheet.css")
         (:title (str title))
         (str (generate-prologue ajax-processor))
         (:script :type "text/javascript"
                  (str (client-side-methods))))

        (:body
         (:h1 (str title))
         (:script :src "https://d3js.org/d3.v4.min.js")
         (:script :src "display.js")))))))


