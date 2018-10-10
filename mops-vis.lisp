(asdf:load-system "KaBOB")


(defpackage :mops-vis
  (:use :cl :net-vis :KaBOB :mops)
  (:export :start-website :mops-to-json :initial-mop))

(in-package :KaBOB)
(defun KaBOB:lookup-mop (name)
  (lookup-mop-by-uniprot-id name))

(in-package :mops-vis)

(KaBOB:open-KaBOB)
;(enable-!-reader)
;(mopify (bio *p53*))

;;;;;;;;; JSON ;;;;;;;;;;

;;; Conversion of MOPs to JSON

(defun make-filler-link (mop slot filler)
  (let ((string-filler (cond ((mops:mop-p filler) (mops:mop-name filler))
                             ((listp filler) (mapcar #'mops:mop-name filler))
                             (t filler))))
    (net-vis:make-link :source mop
               :label (mops:slot-role slot)
               :target string-filler)))

(defun make-slot-links (mop slot)
  (mapcar (lambda (filler) (make-filler-link mop slot filler)) (mops:slot-filler slot)))

(defun make-mop-links (mop)
  (mapcan (lambda (slot) (make-slot-links mop slot)) (mops:mop-slots mop)))

(defun make-mop-node (mop)
  (net-vis:make-node (mops:mop-name mop)))

(defun make-mop-nodes (mop)
  (cons (make-mop-node mop)
        (mapcar (lambda (abstraction) (make-mop-node abstraction)) (mops:mop-abstractions mop))))

(defun mops-to-json (mops)
  (let ((nodes (mapcan #'make-mop-nodes mops))
        (links (mapcan #'make-mop-links mops)))
    (net-vis:make-json-graph nodes links)))

(defvar initial-mop (KaBOB:lookup-mop 'P04637))

;;; Overriding methods
(in-package :net-vis)

(defun send-initial-graph ()
  (format t "graph requested~%")
  (mops-vis:mops-to-json (list mops-vis:initial-mop)))

(defun send-node-data (node-name)
  (let ((mop (KaBOB:lookup-mop (format nil "|~a|" node-name))))
    (mops-vis:mops-to-json (list mop))))
