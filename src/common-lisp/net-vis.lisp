(in-package :net-vis)

;;;;;;;;; Site ;;;;;;;;;

(setq cl-who:*attribute-quote-char* #\")
(defvar *server*)

(defun start-server (&key (port 8081) (dir "~/code/common-lisp/mops-vis/"))
  (let ((index-file  (merge-pathnames "mops.html" dir))
        (javascript (merge-pathnames "src/js/" dir))
        (resources (merge-pathnames "resources/" dir)))

    (initialize-ajax)
    (setq *server* (make-instance 'easy-acceptor :port port))
    (start *server*)

    (push (create-folder-dispatcher-and-handler
           "/resources/" resources)
          *dispatch-table*)
    (push (create-folder-dispatcher-and-handler
           "/JavaScript/" javascript)
          *dispatch-table*)

    (push (create-static-file-dispatcher-and-handler
           "/mops" index-file)
          *dispatch-table*)
    (format t "Using: ~a~%~a:~a/mops"
            index-file
            (if (acceptor-address *server*)
                (acceptor-address *server*)
                "localhost")
            (acceptor-port *server*))))

(defun stop-server ()
  (stop *server*))

;;;;;;;;; JSON ;;;;;;;;

;;; Making streams
(defun make-json-graph (nodes links)
  (cl-json:encode-json-to-string
   (pairlis '("nodes" "links") (list nodes links) (list '(bar . t)))))

;;; Making elements
(defun make-node (name)
  (pairlis '("id" "name") (list name name) (list)))

(defun make-link (&key source label target data)
  (pairlis '("source" "label" "target" "data") (list source label target data)))

;;;;;;;; AJAX ;;;;;;;;

;;; Client side methods (calling methods)
;;; They need to be capitalized because Smackjack uses Parenscript which capitalizes everything
(defun initialize-ajax ()
  (let ((ajax-processor (make-instance 'ajax-processor :server-uri "/ajax-process")))

    (defun-ajax GET-NODE (node-name get-inherited) (ajax-processor :callback-data :json)
      (send-node-data node-name get-inherited))

    (defun-ajax GET-AUTOCOMPLETE () (ajax-processor :callback-data :json)
      (cl-json:encode-json-to-string *auto-complete-data*))

    (defun-ajax GET-SEARCH-RESULTS (ids search-type) (ajax-processor :callback-data :json)
      (send-search-results ids search-type))

    (setq *dispatch-table* (list 'dispatch-easy-handlers
                                 (create-ajax-dispatcher ajax-processor)))
    (generate-prologue ajax-processor)))

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

(defun make-nodes (node-data data)
  (format t "~a~%" data)
  (cons (make-node (car node-data)) (mapcar #'make-node (car (cdr node-data)))))

(defun make-links (node-data data)
  (format t "~a~%" data)
  (mapcar (lambda (target) (make-link :source (first node-data)
                                      :label ""
                                      :target target))
          (second node-data)))

(defun send-node-data (node-name &rest data)
  (let ((node-data (assoc node-name full-data :test #'string=)))
    (make-json-graph (make-nodes node-data data) (make-links node-data data))))

(defun send-search-results (ids search-type)
  (format t "~a, ~a~%" ids search-type))
