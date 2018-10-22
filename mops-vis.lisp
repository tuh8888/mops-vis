(asdf:load-system "KaBOB")

;; (defpackage :mops-vis
;;   (:use :cl :net-vis :KaBOB :mops)
;;   (:export :start-website :mops-to-json :initial-mop))

(in-package :KaBOB)

(defvar initial-mop (lookup-mop-by-uniprot-id 'P04637))

;(in-package :mops-vis)

(open-KaBOB)
;(enable-!-reader)
;(mopify (bio *p53*))


;;;;;;;;; JSON ;;;;;;;;;;

(defun stringify (x)
  (cond ((stringp x) x)
	((mops:mop-p x) (symbol-name (mop-name x)))
	((listp x) (format nil "~{~a~^, ~}" (mapcar #'stringify x)))
	(t (format nil "~a" x))))

(defun make-data (inherited)
  (format nil "~a=~a" "inherited" (if inherited "true" "false")))

(defun inherited-role? (mop role)
  (if (find role (mop-roles mop)) t nil))

(defun make-link (mop role filler inherited)
  (net-vis:make-link :source (stringify mop)
		     :label (stringify role)
		     :target (stringify filler)
                     :data (make-data inherited)))

(defun make-role-links (mop role)
  (let ((fillers (inherit-filler mop role)))
    (cond ((listp fillers) (mapcar #'(lambda (filler) (make-link mop role filler (inherited-role? mop role))) (inherit-filler mop role)))
          (t (list (make-link mop role fillers (inherited-role? mop role)))))))

(defun make-abstraction-links (mop)
  (mapcar #'(lambda (abstraction) (make-link mop "subClassOf" abstraction nil)) (mop-abstractions mop)))

(defun make-mop-links (mop get-inherited)
  `(,@(make-abstraction-links mop)
    ,@(mapcan #'(lambda (role) (make-role-links mop role)) (if get-inherited
                                                               (mop-roles mop)
                                                               (inheritable-roles mop)))))

(defun make-mop-node (x)
  (net-vis:make-node (stringify x)))

(defun make-filler-nodes (mop role)
  (let ((fillers (inherit-filler mop role)))
    (cond ((listp fillers) (mapcar #'make-mop-node fillers))
          (t (list (make-mop-node fillers))))))

(defun make-mop-nodes (mop get-inherited)
  `(,(make-mop-node mop)
    ,@(mapcar #'make-mop-node (mop-abstractions mop))
    ,@(mapcan #'(lambda (role) (make-filler-nodes mop role)) (if get-inherited
                                                                 (mop-roles mop)
                                                                 (inheritable-roles mop)))))

(defun mops-to-json (mops get-inherited)
  (net-vis:make-json-graph (mapcan #'(lambda (mop) (make-mop-nodes mop get-inherited)) mops) (mapcan #'(lambda (mop) (make-mop-links mop get-inherited)) mops)))

(defun find-node-data (node-name)
  (lookup-mop (intern node-name :KaBOB)))

;;; Overriding methods
(in-package :net-vis)

(defun send-initial-graph ()
  (format t "graph requested~%")
  (KaBOB::mops-to-json (list KaBOB::initial-mop)))

(defun send-node-data (node-name get-inherited)
  (format t "node requested: ~a~%" node-name)
  (KaBOB::mops-to-json (list (KaBOB::find-node-data node-name)) get-inherited))
