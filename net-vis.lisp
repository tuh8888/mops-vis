(ql:quickload '("hunchentoot" "cl-who" "smackjack"))

(defpackage :net-vis
  (:use :hunchentoot :cl-who :cl-json :cl :smackjack)
  (:export :start-website :make-node :make-link :make-json-graph :*server* :send-node-data
           :send-initial-graph))

(in-package :net-vis)

;;;;;;;;; Site ;;;;;;;;;

(setq cl-who:*attribute-quote-char* #\")
(defvar *server*)
(defvar local-dir "~/code/common-lisp/mops-vis/")
(defvar index-file  (merge-pathnames "index.html" local-dir))
(defvar display-file (merge-pathnames "main.js" local-dir))
(defvar stylesheet-file (merge-pathnames "stylesheet.css" local-dir))

(defun start-website (&key (page-uri "index") (port 8081))
  (let ((ajax-processor (initialize-ajax)))
    (setq *server* (make-instance 'easy-acceptor :port port))
    (start *server*)
    (make-page page-uri ajax-processor)))

(defun make-page (page-uri ajax-processor)
  (push (create-static-file-dispatcher-and-handler
         "/stylesheet.css" stylesheet-file)
        *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/main.js" display-file)
        *dispatch-table*)

  (push (create-static-file-dispatcher-and-handler
         "/index" index-file)
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
  ;;         (:script :src "main.js"))))))
  (format t "Page: ~a~%Using: ~a~%" page-uri index-file))

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
;;; They need to be capitalized because Smackjack uses Parenscript which capitalizes everything
(defun initialize-ajax ()
  (let ((ajax-processor (make-instance 'ajax-processor :server-uri "/ajax-process")))

    (defun-ajax GET-NODE (node-name) (ajax-processor :callback-data :json)
      (send-node-data node-name))

    (defun-ajax GET-INITIAL-GRAPH () (ajax-processor :callback-data :json)
      (send-initial-graph))

    (setq *dispatch-table* (list 'dispatch-easy-handlers
                                 (create-ajax-dispatcher ajax-processor)))

    ajax-processor))

;;; Server side methods (response methods)
;;; These should be overridden by the implementation
;;; They are provided here as an example

(defvar initial-data '(("1" ("2" "3"))
                       ("2" ("4" "5"))
                       ("3" ())
                       ("4" ())
                       ("5" ("1"))))

(defvar full-data '(("1" ("2" "3"))
                    ("2" ("4" "5"))
                    ("3" ())
                    ("4" ())
                    ("5" ("1" "6"))
                    ("6" ("7"))))

(defun make-nodes (node-data)
  (cons (make-node (car node-data)) (mapcar #'make-node (car (cdr node-data)))))

(defun make-links (node-data)
  (mapcar (lambda (target) (make-link :source (first node-data)
                                      :label ""
                                      :target target))
          (second node-data)))

(defun send-node-data (node-name)
  (let ((node-data (assoc node-name full-data :test #'string=)))
    (make-json-graph (make-nodes node-data) (make-links node-data))))

(defun send-initial-graph ()
  (format t "graph requested~%")

  (let ((nodes (mapcan #'make-nodes initial-data))
        (links (mapcan #'make-links initial-data)))
    (make-json-graph nodes links)))
