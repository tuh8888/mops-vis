(in-package :net-vis)

(defun make-autocomplete-tree (words)
  (merge-with (mapcar #'make-word-tree words)))

(defun make-word-tree (word)
  (do* ((i (- (length word) 1) (- i 1))
        (char (aref word i) (aref word i))
        (node (pairlis (list char "bar") (list (list '(:stop . "foo")) "foo") nil) (acons char node nil)))
       ((= i 0) node)))

(defun merge-with (trees)
  (let ((tree (car trees))
        (rest (cdr trees)))
    (dolist (current (reduce #'append rest) tree)
      (cond ((equal :stop current) (push (list :stop) tree))
            (t 
             (let* ((current-key (car current))
                    (res-key-value (assoc current-key tree :test #'equal))
                    (res-value (cdr res-key-value)))
               (cond ((null res-value) (push current tree))
                     ((equal "foo" res-value) (push current tree))
                     (t
                      (rplacd res-key-value
                              (merge-with (list res-value (cdr current))))))))))))

(defun make-autocomplete-tree-from-map (hashmap)
  
  (let ((keys nil))
    (with-hash-table-iterator (next-entry hashmap)
      (loop (multiple-value-bind (more key) (next-entry)
             (unless more (return nil))
              (push (symbol-name key) keys))))
    
    (make-autocomplete-tree keys)))

(defvar *autocomplete-data* nil)
