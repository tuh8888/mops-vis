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

(defun filler-to-string (filler)
  (cond ((mops:mop-p filler) (symbol-name (mops:mop-name filler)))
        ((listp filler) (format nil "~{~a~^, ~}" (mapcar (lambda (filler-item)  (symbol-name (mops:mop-name filler-item))) filler)))
        (t filler)))


;;; Conversion of MOPs to JSON

(defun make-filler-link (mop slot filler)
  (net-vis:make-link :source (symbol-name (mops:mop-name mop))
                     :label (mops:slot-role slot)
                     :target (filler-to-string filler)))

(defun make-slot-links (mop slot)
  (mapcar (lambda (filler) (make-filler-link mop slot filler)) (mops:slot-filler slot)))


(defun make-abstraction-link (mop abstraction)
  (net-vis:make-link :source (symbol-name (mops:mop-name mop))
                     :label "subclassof"
                     :target (symbol-name (mops:mop-name abstraction))))

(defun make-mop-links (mop)
  (append (mapcar (lambda (abstraction) (make-abstraction-link mop abstraction)) (mops:mop-abstractions mop))
          (mapcan (lambda (slot) (make-slot-links mop slot)) (mops:mop-slots mop))))

(defun make-filler-node (filler)
  (net-vis:make-node (filler-to-string filler)))

(defun make-slot-nodes (mop slot)
  (mapcar #'make-filler-node (mops:slot-filler slot)))

(defun make-mop-node (mop)
  (net-vis:make-node (symbol-name (mops:mop-name mop))))

(defun make-mop-nodes (mop)
   (cons (make-mop-node mop)
         (append (mapcar (lambda (abstraction) (make-mop-node abstraction)) (mops:mop-abstractions mop))
               (mapcan (lambda (slot) (make-slot-nodes mop slot)) (mops:mop-slots mop)))))

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
