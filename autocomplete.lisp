(defvar examples '("hello" "there" "appletree" "how" "are" "the" "ants" "and" "their" "apples" "there" "?"))

(defun make-autocomplete-tree (words)
  (merge-with (mapcar #'make-word-tree words)))

(defun make-word-tree (word)
  (do* ((i (- (length word) 1) (- i 1))
        (char (aref word i) (aref word i))
        (node (acons char (list (list :stop)) nil) (acons char node nil)))
       ((= i 0) node)))

(defun merge-with (trees)
  (let ((tree (car trees))
        (rest (cdr trees)))
    (dolist (current (reduce #'append rest) tree)
      (if (equal :stop current)
          (push (list :stop) tree)
          (let* ((current-key (car current))
                 (res-key-value (assoc current-key tree :test #'equal))
                 (res-value (cdr res-key-value)))
            (if (null res-value)
                (push current tree)
                (rplacd res-key-value
                        (merge-with (list res-value (cdr current))))))))))

(defparameter *my-hash* (make-hash-table))
(setf (gethash 'first-key *my-hash*) 'one)
(setf (gethash 'second-key *my-hash*) 'two)
(setf (gethash 'third-key *my-hash*) 'three)

(defun make-kabob-autocomplete ()
  (make-autocomplete-tree (maphash #'(lambda (key value) (symbol-name key)) *my-hash*)))

(make-kabob-autocomplete)
