(asdf:load-system "KaBOB")

(in-package :KaBOB)

;;; UTIL ;;;

(defun vis-roles (mop data)
  (filter-roles (if (assoc "inherited" data :equal)                    
                    (inheritable-roles mop)
                    (mop-roles mop))))

(defun filter-roles (roles)
  (remove-if #'(lambda (role) (equal :kabob-ids role)) roles))

(defun inherited-role? (mop role)
  (if (find role (mop-roles mop)) t nil))

(defun find-node (node-name)
  (lookup-mop (intern node-name :KaBOB)))

(defun find-relationship (source target)
  (let ((relationship nil))
    (dolist (role (mop-roles source) 'done)
      (dolist (filler (role-filler source role) 'done)
        (when (equal filler target)
          (setf relationship role))))
    (unless relationship
      (dolist (abstraction (mop-abstractions source) 'done)
        (when (equal abstraction target)
          (setf relationship "subClassOf"))))
    relationship))


;;; LINKS ;;;

(defun make-mops-links (mops data)
  (mapcan #'(lambda (mop) (make-mop-links mop data)) mops))

(defun make-mop-links (mop data)
  `(,@(make-abstraction-links mop)
    ,@(mapcan #'(lambda (role)
                  (make-role-links mop role))
              (vis-roles mop data))))

(defun make-abstraction-links (mop)
  (mapcar #'(lambda (abstraction)
              (net-vis:make-link :source mop :label "subClassOf" :target abstraction :data nil))
          (mop-abstractions mop)))

(defun make-role-links (mop role)
  (let ((fillers (inherit-filler mop role)))
    (if (listp fillers)
        (mapcar #'(lambda (filler)
                    (net-vis:make-link :source mop :label role :target filler :data (inherited-role? mop role)))
                (inherit-filler mop role))
        (list (net-vis:make-link :source mop :label role :target fillers :data (inherited-role? mop role))))))

(defun make-path-link (source target)
  (let ((relationship (find-relationship source target)))
    ; Check if there is a relationship. Otherwise, assume the directionality is reversed
    (if relationship
        (net-vis:make-link :source source :label relationship :target target :data nil)
        (net-vis:make-link :source target :label (find-relationship target source) :target source :data nil))))


;;; NODES ;;;

(defun make-mops-nodes (mops data)
  (mapcan #'(lambda (mop) (make-mop-nodes mop data)) mops))

(defun make-mop-nodes (mop data)
  `(,(net-vis:make-node mop)
    ,@(mapcar #'net-vis:make-node (mop-abstractions mop))
    ,@(mapcan #'(lambda (role) (make-filler-nodes mop role)) (vis-roles mop data))))

(defun make-filler-nodes (mop role)
  (let ((fillers (inherit-filler mop role)))
    (cond ((listp fillers) (mapcar #'net-vis:make-node fillers))
          (t (list (net-vis:make-node fillers))))))


;;; SEARCH ;;;

(defun run-search (mops type parameters)
  (cond ((equal type "intersection search")
        (intersection-search mops #'mop-neighbors :max-depth 2 :max-fanout 8)
         )))

(setq mops (mapcar #'find-node (list "cellular tumor antigen p53 (human)"
                                     "cellular tumor antigen p53")))
(setq paths (run-search mops "intersection search"))

;;;;;;;;;;;;;;;;;;;;;;; NET-VIS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Override net-vis methods
(in-package :net-vis)


;;; UTIL ;;;
(defun make-data (inherited)
  (format nil "~a=~a" "inherited" (if inherited "true" "false")))

(defun stringify (x)
  (cond ((stringp x) x)
	((KaBOB::mop-p x) (symbol-name (KaBOB::mop-name x)))
	((listp x) (format nil "~{~a~^, ~}" (mapcar #'stringify x)))
	(t (format nil "~a" x))))

(defun process-paths (paths)
  (do ((path (car paths) (car rest))
       (rest paths (cdr rest))
       (nodes nil (append (mapcar #'make-node path) nodes))
       (links nil (append (process-path-links path) links)))

      ((null rest) (make-json-graph nodes links))))

(defun process-path-links (path)
  (mapcar #'(lambda (previous-mop mop)
              (KaBOB::make-path-link previous-mop mop))
          path (cdr path)))

;;; SEND ;;;
(defun send-node-data (node-name data)
  (format t "node requested: ~a~%data requested: ~a~%" node-name data)
  (let ((mops (list (KaBOB::find-node node-name))))
    (make-json-graph
     (KaBOB::make-mops-nodes mops data)
     (KaBOB::make-mops-links mops data))))

(defun send-search-results (ids type parameters)
  (format t "search requested~%ids: ~a~%type: ~a~%parameters: ~a~%" ids type parameters)
  (process-paths (KaBOB::run-search (mapcar #'KaBOB::find-node ids) type parameters)))


;;; AUTOCOMPLETE ;;;
;(setf *auto-complete-data* (make-autocomplete-tree-from-map KaBOB::*mops*))

