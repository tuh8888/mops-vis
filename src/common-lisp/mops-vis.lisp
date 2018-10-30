(asdf:load-system "KaBOB")

(in-package :KaBOB)

;;;;;;;;; JSON ;;;;;;;;;;

(defun inherited-role? (mop role)
  (if (find role (mop-roles mop)) t nil))

(defun make-role-links (mop role)
  (let ((fillers (inherit-filler mop role)))
    (cond ((listp fillers) (mapcar #'(lambda (filler) (make-link :source mop
                                                                 :label role
                                                                 :taraget filler
                                                                 :data (inherited-role? mop role)))
                                   (inherit-filler mop role)))
          (t (list (make-link :source mop
                              :label role
                              :target fillers
                              :data (inherited-role? mop role)))))))

(defun make-abstraction-links (mop)
  (mapcar #'(lambda (abstraction) (make-link :source mop
                                             :label "subClassOf"
                                             :target abstraction
                                             :data nil))
          (mop-abstractions mop)))

(defun make-mop-links (mop get-inherited)
  `(,@(make-abstraction-links mop)
    ,@(mapcan #'(lambda (role) (make-role-links mop role)) (if get-inherited
                                                               (mop-roles mop)
                                                               (inheritable-roles mop)))))

(defun make-filler-nodes (mop role)
  (let ((fillers (inherit-filler mop role)))
    (cond ((listp fillers) (mapcar #'make-node fillers))
          (t (list (make-node fillers))))))

(defun make-mop-nodes (mop get-inherited)
  `(,(make-node mop)
    ,@(mapcar #'make-node (mop-abstractions mop))
    ,@(mapcan #'(lambda (role) (make-filler-nodes mop role)) (if get-inherited
                                                                 (mop-roles mop)
                                                                 (inheritable-roles mop)))))

(defun make-mops-nodes (mops get-inherited)
  (mapcan #'(lambda (mop) (make-mop-nodes mop get-inherited)) mops))

(defun make-mops-links (mops get-inherited)
  (mapcan #'(lambda (mop) (make-mop-links mop get-inherited)) mops))

(defun find-node (node-name)
  (lookup-mop (intern node-name :KaBOB)))

(defun run-search (ids search-type)
  (cond ((equal search-type "intersection search")
         (intersection-search (mapcar #'find-node ids) #'mop-neighbors :max-depth 2 :max-fanout 8))))

(defun find-relationship (source target)
  (let ((relationship nil))
    (dolist (role (mop-roles source) 'done)
     (dolist (filler (role-filler source role) 'done)
       (when (equal filler target)
         (setf relationship role))))
    (unless relationship
      (dolist (abstraction (mop-abstractions source) 'done)
        (when (equal abstraction target)
          (setf-relationship "subClassOf"))))
    relationship))

(defun make-path-link (source target)
  (make-link :source source
             :label (KaBOB::find-relationship source target)
             :target target
             :data nil))


;;; Override net-vis methods
(in-package :net-vis)

(defun make-data (inherited)
  (format nil "~a=~a" "inherited" (if inherited "true" "false")))

(defun stringify (x)
  (cond ((stringp x) x)
	((mop-p x) (symbol-name (mop-name x)))
	((listp x) (format nil "~{~a~^, ~}" (mapcar #'stringify x)))
	(t (format nil "~a" x))))

(defun send-node-data (node-name get-inherited)
  (format t "node requested: ~a~%" node-name)
  (let ((mops (list (KaBOB::find-node node-name))))
    (make-json-graph
     (KaBOB::make-mops-nodes mops get-inherited)
     (KaBOB::make-mops-links mops get-inherited))))

(defun send-search-results (ids search-type)
  (process-paths (run-search ids search-type)))

(defun process-paths (paths)
  (do ((path (car paths) (car rest))
       (rest paths (cdr rest))
       (nodes nil nodes)
       (links nil links))

      ((null rest) (make-json-graph nodes links))
    (process-path path nodes links)))

(defun process-path (path nodes links)
  (do ((mop (car path) (car path-rest))
       (path-rest path (cdr path))
       (previous-mop nil mop))

      ((null path-rest) 'done)
    
    (push (make-node mop) nodes)
    ;; (when previous-mop
      ;; (push (make-path-link previous-mop mop) links))
    ))

(setf *auto-complete-data* (make-autocomplete-tree-from-map KaBOB::*mops*))
