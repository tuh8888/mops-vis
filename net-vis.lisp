(in-package :net-vis)

;;;;;;;;; Site ;;;;;;;;;

(setq cl-who:*attribute-quote-char* #\")
(defvar *server*)
(defvar local-dir "~/code/common-lisp/mops-vis/")
(defvar index-file  (merge-pathnames "mops.html" local-dir))
(defvar javascript (merge-pathnames "JavaScript/" local-dir))
(defvar resources (merge-pathnames "resources/" local-dir))

(defun start-website (&key (page-uri "index") (port 8081))
  (let ((ajax-processor (initialize-ajax)))
    (setq *server* (make-instance 'easy-acceptor :port port))
    (start *server*)
    (make-page page-uri ajax-processor)))

(defun make-page (page-uri ajax-processor)
  (push (create-folder-dispatcher-and-handler
         "/resources/" resources)
        *dispatch-table*)
  (push (create-folder-dispatcher-and-handler
         "/JavaScript/" javascript)
        *dispatch-table*)

  (push (create-static-file-dispatcher-and-handler
         "/mops" index-file)
        *dispatch-table*)
  (format t "Page: ~a~%Using: ~a~%" page-uri index-file))

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
