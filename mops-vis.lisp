(asdf:load-system "KaBOB")


(defpackage :mops-vis
  (:use :cl :net-vis :KaBOB)
  (:export :start-website))

(in-package :KaBOB)

(open-KaBOB)
(enable-!-reader)
(mopify (bio *p53*))

;;;;;;;;; JSON ;;;;;;;;;;

;;; Conversion of MOPs to JSON

(defun make-filler-link (mop slot filler)
  (let ((string-filler (cond ((mop-p filler) (mop-name filler))
                             ((listp filler) (mapcar #'mop-name filler))
                             (t filler))))
    (net-vis:make-link :source mop
               :label (slot-role slot)
               :target string-filler)))

(defun make-slot-links (mop slot)
  (mapcar (lambda (filler) (make-filler-link mop slot filler)) (slot-filler slot)))

(defun make-mop-links (mop)
  (mapcan (lambda (slot) (make-slot-links mop slot)) (mop-slots mop)))

(defun make-mop-node (mop)
  (net-vis:make-node (mop-name mop)))

(defun make-mop-nodes (mop)
  (cons (make-mop-node mop)
        (mapcar (lambda (abstraction) (make-mop-node abstraction)) (mop-abstractions mop))))

(defun mops-to-json (mops)
  (let ((nodes (mapcan #'make-mop-nodes mops))
        (links (mapcan #'make-mop-links mops)))
    (net-vis:make-json-graph nodes links)))

(defvar initial-mop (lookup-mop-by-uniprot-id 'P04637))

;;; Overriding methods
(in-package :net-vis)

(defun send-initial-graph ()
  (format t "graph requested~%")
  (mops-to-json (list initial-mop)))

(defun send-node-data (node-name)
  (let ((mop (KaBOB:lookup-mop (format nil "|~a|" node-name))))
    (mops-to-json (list mop))))
