(ql:quickload '("hunchentoot" "cl-who" "smackjack"))

(defpackage :web-net
  (:use :hunchentoot :cl-who :cl-json :cl :smackjack)
  (:export :start-website :make-node :make-link :make-json-graph))

(in-package :web-net)

;;;;;;;;; Site ;;;;;;;;;

(setq cl-who:*attribute-quote-char* #\")

(defun start-website (&optional (page-uri "index"))
  (let ((ajax-processor (initialize-ajax)))
    (start (make-instance 'easy-acceptor :port 8080))
    (make-page page-uri ajax-processor)))

(defun make-page (page-uri ajax-processor)
  (push (create-static-file-dispatcher-and-handler
         "/stylesheet.css" (merge-pathnames "stylesheet.css" *load-truename*))
        *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/display.js" (merge-pathnames "display.js" *load-truename*))
        *dispatch-table*)

  (push (create-static-file-dispatcher-and-handler
         "/index" (merge-pathnames "index.html" *load-truename*))
        *dispatch-table*)
  
  
  ;; (let ((title "Network Visualization"))
  ;;   (define-easy-handler (network-display :uri (concatenate 'string "/" page-uri)) ()
  ;;     (with-html-output-to-string (s)
  ;;       (:html
  ;;        (:head
  ;;         (:link :rel "stylesheet" :href "stylesheet.css")
  ;;         (:title (str title)))

  ;;        (:body
  ;;         (:h1 (str title))
  ;;         (:script :src "https://d3js.org/d3.v4.min.js")
  ;;         (:script :src "display.js"))))))
  page-uri
  )

;;;;;;;;; JSON ;;;;;;;;

;;; Making streams
(defun make-json-graph (nodes links)
  (cl-json:encode-json-to-string
   (pairlis '("nodes" "links") (list nodes links) (list '(bar . t)))))

;;; Making elements
(defun make-node (name)
  (pairlis '("id" "name") (list name name) (list)))

(defun make-link (&key source label target)
  (pairlis '("source" "label" "target") (list source label target)))

;;;;;;;; AJAX ;;;;;;;;

;;; Client side methods (calling methods)
(defun initialize-ajax ()
  (let ((ajax-processor (make-instance 'ajax-processor :server-uri "/ajax-process")))

    (defun-ajax get-node (node-name) (ajax-processor :callback-data :json)
      (send-links node-name))

    (defun-ajax get-initial-graph () (ajax-processor :callback-data :json)
      (send-initial-graph))

    (setq *dispatch-table* (list 'dispatch-easy-handlers
                                 (create-ajax-dispatcher ajax-processor)))

    ajax-processor))

;;; Server side methods (response methods)
;;; These should be overridden by the implementation
;;; They are provided here as an example

(defvar initial-data '((1 (2 3))
                       (2 (4 5))
                       (3 ())
                       (4 ())
                       (5 (1))))

(defvar full-data '((1 (2 3))
                    (2 (4 5))
                    (3 ())
                    (4 ())
                    (5 (1 6))
                    (6 (7))))

;;; TODO I need to know what links and nodes already exist
(defun send-links (node-name)
  (let ((node-data (assoc node-name full-data)))
    (make-json-graph (list (make-node (car node-data))) (mapcar (lambda (target) (make-link :source (first node-data)
                                                                                            :label ""
                                                                                            :target target))
                                                                (second node-data)))))

(defun send-initial-graph ()
  (format t "graph requested~%")
  (let ((nodes (mapcar #'(lambda (node)
                               (make-node (first node)))
                           initial-data))
        (links (mapcan (lambda (pair)
                         (mapcar (lambda (target)
                                   (make-link :source (first pair)
                                              :label ""
                                              :target target))
                                 (second pair)))
                       initial-data)))
    (make-json-graph nodes links)))

(start-website)








