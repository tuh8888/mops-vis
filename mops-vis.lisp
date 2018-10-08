(asdf:load-system "KaBOB")


(defpackage :kabob-display
  (:use :cl :web-net :KaBOB)
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
    (web-net:make-link :source mop
               :label (slot-role slot)
               :target string-filler)))

(defun make-slot-links (mop slot)
  (mapcar (lambda (filler) (make-filler-link mop slot filler)) (slot-filler slot)))

(defun make-mop-links (mop)
  (mapcan (lambda (slot) (make-slot-links mop slot)) (mop-slots mop)))

(defun make-mop-node (mop)
  (web-net:make-node (mop-name mop)))

(defun make-mop-nodes (mop)
  (cons (make-mop-node mop)
        (mapcar (lambda (abstraction) (make-mop-node abstraction)) (mop-abstractions mop))))

(defun mops-to-json (mops)
  (let ((nodes (mapcan #'make-mop-nodes mops))
        (links (mapcan #'make-mop-links mops)))
    (web-net:make-json-graph nodes links)))

;;; Testing JSON conversion


(defun get-initial-graph ()
  (format t "graph requested~%")
  (let ((mop (lookup-mop-by-uniprot-id 'P04637)))
    (mops-to-json (list mop))))

(defun get-node (node-name)
  (let ((mop (find-mop (format nil "|~a|" node-name))))
    (mops-to-json (list mop))))


(defun get-graph ()
  "")

(start-website)
