(asdf:load-system "KaBOB")


(defpackage :mops-vis
  (:use :cl :net-vis :KaBOB)
  (:export :start-website))

(in-package :mops-vis)

(KaBOB:open-KaBOB)
;(KaBOB:enable-!-reader)
;(KaBOB:mopify (bio *p53*))

;;;;;;;;; JSON ;;;;;;;;;;

;;; Conversion of MOPs to JSON

(defun make-filler-link (mop slot filler)
  (let ((string-filler (cond ((KaBOB:mop-p filler) (KaBOB:mop-name filler))
                             ((listp filler) (mapcar #'KaBOB:mop-name filler))
                             (t filler))))
    (net-vis:make-link :source mop
               :label (KaBOB:slot-role slot)
               :target string-filler)))

(defun make-slot-links (mop slot)
  (mapcar (lambda (filler) (make-filler-link mop slot filler)) (KaBOB:slot-filler slot)))

(defun make-mop-links (mop)
  (mapcan (lambda (slot) (make-slot-links mop slot)) (KaBOB:mop-slots mop)))

(defun make-mop-node (mop)
  (net-vis:make-node (KaBOB:mop-name mop)))

(defun make-mop-nodes (mop)
  (cons (make-mop-node mop)
        (mapcar (lambda (abstraction) (make-mop-node abstraction)) (KaBOB:mop-abstractions mop))))

(defun mops-to-json (mops)
  (let ((nodes (mapcan #'make-mop-nodes mops))
        (links (mapcan #'make-mop-links mops)))
    (net-vis:make-json-graph nodes links)))

;;; Overriding methods
(in-package :net-vis)

(defun send-initial-graph ()
  (format t "graph requested~%")
  (let ((mop (KaBOB:lookup-mop-by-uniprot-id 'P04637)))
    (mops-vis:mops-to-json (list mop))))

(defun send-node-data (node-name)
  (let ((mop (KaBOB:find-mop (format nil "|~a|" node-name))))
    (mops-vis:mops-to-json (list mop))))
