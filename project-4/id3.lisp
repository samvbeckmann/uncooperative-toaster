;;;; ID3.LISP
;;;;     Quinlan'S ID3 Algorithm for constructing decision trees
;;;;     for learning from examples.
;;;;     (See article by Quinlan in ML 1:1, 1986.)

;;;; Copyright (C) 1988, 1989 by Jude William Shavlik.
;;;;         This program may be freely copied, used, or 
;;;;         modified provided that this copyright notice 
;;;;         is included in each copy of this code and parts thereof.

;;;; This implementation of ID3 produces decision trees descriminating positive
;;;; and negative instances.  Instances are represented by simple nominal features.
;;;; A keyword rather than a positional representation is used.  
;;;; The function RUN-ID3 is called to generate the decision tree and evaluate
;;;; the decision tree.

;;; ---------------------------------------------------------------------------
;;;                     Global variables used by ID3.
;;; ---------------------------------------------------------------------------

(defvar *trace-id3*        t "Produces a trace of operation if set.")
(defvar *current-tree*   nil "The most recently learned decision tree.")
(defvar *train-examples* nil "Training examples - see BUILD-EXAMPLE-LISTS.")
(defvar *test-examples*  nil "Testing examples - see BUILD-EXAMPLE-LISTS.")

(load "utils.lisp")

;;; ---------------------------------------------------------------------------
;;;                        The Main Functions
;;; ---------------------------------------------------------------------------

;;; I MODIFIED THIS
(defun id3 (examples possible-attributes splitting-function)
  "A simple version of Quinlan's ID3 Program - see Machine Learning 1:1, 1986.
   Numeric feature values, noisy data and missing values are not handled."
 ;;; This function produces a decision tree that classifies the examples 
 ;;; provided, using these attributes.  The splitting function determines
 ;;; which attribute should be used to split a collection of + and - examples.
 ;;; If all the examples are of the same type, a leaf node is returned.

 ;;; The resulting decision tree is a list of the form 
 ;;;     (attribute (value1 subtree1) (value2 subtree2) ... ) 
 ;;;  or (decision  #-of-examples-in-training-set-located-here)
 ;;; In the first case, depending on the value of attribute, another decision
 ;;; tree must be traversed to make a decision.  In the second case, a
 ;;; decision is recorded, along with the number of examples from the
 ;;; training set that would be placed at this node.

 ;;  See RUN-ID3 for a nice user interface to this function.

 ;;  It is assumed that every example has a valid value for each attribute.
 ;;  The function VALIDATE-EXAMPLES can be used to pre-process examples. 

     (cond ((null examples) '(? 0))  ; No more examples, an "undecided" node.
           ((all-positive? examples) `(+ , (length examples)))
           ((all-negative? examples) `(- , (length examples)))
           ((null possible-attributes) (error "Out of features - inconsistent data."))
           (t (let ((chosen-attribute (choose-attribute examples possible-attributes splitting-function)) (result ()))
                (dolist (value (get-attribute-values chosen-attribute) (cons chosen-attribute result))
                  (push (list value (id3 (collect-examples-with-this-value examples chosen-attribute value)
                                         (remove chosen-attribute possible-attributes) splitting-function)) result))))))

;; choose attribute to branch on by calling choose-attribute
;; remove the chosen attribute from the possible-attributes to get remaining attributes
;; return a list of the form 
;;          (chosen-attribute (value1 subtree1) (value2 subtree2) ... ) 
;; where the values are the different values that chosen-attribute can take
;; and the subtrees are created by recursive calls to id3 where the first argument is
;; a subset of the examples that has the ith value for the attribute chosen-attribute
;; (this can be done by calling the function collect-examples-with-this-value),
;; and the second argument is the remaining attributes as created above
;;; SHOULD BE WRITTEN NOW

(defun choose-attribute (examples attributes splitting-function)
  "Choose an attribute to split these examples into sub-groups.
The method of doing so is specified by the third argument."
  (case splitting-function
    (random      (nth (random (length attributes)) attributes)) ; make an arbitrary choice
    (least-values (least_most attributes 'least-values))  ; choose the attribute with the least possible values
    (most-values  (least_most attributes 'most-values))  ; choose the one with the most
    (max-gain (max_gain examples attributes))  ; use Quinlan's gain measure (pg. 90) to choose  - TO BE WRITTEN 
    (otherwise (error "ERROR - unknown splitting function"))))

(defun least_most (attributes decision)
  (let ((attr_num_pairs (mapcar #' (lambda (attribute)
				    (list (length (get-attribute-values attribute)) attribute))
				   attributes)))
    (case decision
      (least-values (second (assoc (reduce #'min (mapcar #'car attr_num_pairs)) attr_num_pairs)))
      (most-values (second (assoc (reduce #'max (mapcar #'car attr_num_pairs)) attr_num_pairs))))))

;;; I WROTE THIS
(defun make-decision (example &optional (decision-tree *current-tree*) )
  "Use this decision tree to classify this unclassified instance."
    (case (first (get-decision-subtree (get-feature-value example (first decision-tree)) (rest decision-tree)))
      ('+ '+)
      ('- '-)
      ('? (if (> (count-matching-leaves *current-tree* '-)
                (count-matching-leaves *current-tree* '+))
          (setf (car decision-tree) '-)
          (setf (car decision-tree) '+)))
      (otherwise
        (make-decision example (get-decision-subtree (get-feature-value example (first decision-tree)) (rest decision-tree))))))

;;; I WROTE THIS
(defun get-decision-subtree (value possible-matches)
  (second (assoc value possible-matches)))

;;; I WROTE THIS
(defun max_gain (examples attributes)
  "Returns the attribute that, if split on, creates the max entropy gain
from the examples"
  (first (get-min-remainder examples attributes)))

(defun get-min-remainder (examples attributes)
  (if (= (length attributes) 1) (remainder examples (first attributes))
    (get-min-attribute (remainder examples (first attributes)) (get-min-remainder examples (rest attributes)))))

(defun get-min-attribute (one two)
  (if (< (second one) (second two))
    one
    two))

(defun remainder (examples attribute)
  "Calculates the entropy remainder after branching on the given attribute"
  (list attribute (calc-remainder examples attribute (get-attribute-values attribute))))

(defun calc-remainder (examples attribute values)
  (if (null values) 0
    (+ (remainder-with-value examples attribute (collect-examples-with-this-value examples attribute (first values)))
       (calc-remainder examples attribute (rest values)))))

(defun remainder-with-value (examples attribute examples-with-value)
  (if (null examples-with-value)
    0
    (* (/ (length examples-with-value) (length examples))
       (b-function (/ (get-positive-examples examples-with-value) (length examples-with-value))))))

(defun get-positive-examples (examples)
  (cond ((null examples) 0)
        ((positive-example? (first examples)) (1+ (get-positive-examples (rest examples))))
        (t (get-positive-examples (rest examples)))))

(defun b-function (q)
  (if (and (> q 0) (< q 1))
      (* -1 (+ (* q (log q 2)) (* (- 1 q) (log (- 1 q) 2))))
      0))

 ;;     - TO BE WRITTEN 
;; Find the root attribute of the decision-tree
;; For that attribute, find the attribute value in the example
;; Find the subtree in decision-tree corresponding to that value
;; If this subtree is a leaf node (will look like (+) or (-)) return that classification
;; If the leaf node looks like (?), use the following code
;;    (if (> (count-matching-leaves parent_node '-)
;;	     (count-matching-leaves parent_node '+))
;;	  (setf (car new_tree) '-)
;;      (setf (car new_tree) '+)))
;; Otherwise, call make-decision recursively with the subtree identified above

(defun all-positive? (examples)
  "Determine if these are all positive examples."
  (every #'(lambda (example) (positive-example? example)) examples))

(defun all-negative? (examples)
 "Determine if these are all negative examples."
  (every #'(lambda (example) (negative-example? example)) examples))

;;; ---------------------------------------------------------------------------
;;;                  Some useful functions (others contained in utility.lisp)
;;; ---------------------------------------------------------------------------

(defun run-id3 (&optional (examples *train-examples*) (splitting-function 'random)
			  (examples-file "CATEGORIZED.DATA")
			  (report-tree? *trace-id3*) 
		&aux start-time)
  "Check these examples for correctness, build a decision tree, then 
draw the tree (if requested) and, finally, report some statistics about it."
  (when (null examples) 
    (format t "~%~%Constructing the test set ... ")
    (build-example-lists examples-file)
    (format t " ~D training examples produced." (length *train-examples*))
    (setf examples *train-examples*))
  (format t "~%~%Building Decision Tree ...")
  (if (validate-examples examples)
     (progn (setf start-time (get-internal-run-time))
	    (setf *current-tree* (id3 examples *all-attributes* splitting-function))
	    (format t " finished in ~,3F sec.~%" 
		    (convert-to-sec (- (get-internal-run-time) start-time)))
            (if report-tree? (print-decision-tree))
            (let ( (interior-nodes (count-interior-nodes *current-tree*))
                   (leaf-nodes     (count-leaf-nodes *current-tree*)))
              (format t "~%~%Tree size=~A    interior nodes=~A  leaf-nodes=~A~%" 
                 (+ interior-nodes leaf-nodes) interior-nodes leaf-nodes))
            (format t "   positive leaves=~A  negative leaves=~A  undecided leaves=~A ~%~%"
              (count-matching-leaves *current-tree* '+)
              (count-matching-leaves *current-tree* '-)
              (count-matching-leaves *current-tree* '?))
	    (measure-correctness-ID3 *test-examples*))
     (format t "~%~% RUN ABORTED DUE TO ERRONEOUS TRAINING DATA.~%~%")))

(defun measure-correctness-ID3 (&optional (examples *test-examples*) (report-progress? *trace-id3*)
					  &aux start-time (correct 0) (total (length examples))
					       (pos 0) (neg 0) (pos_corr 0) (neg_corr 0))
  "Test the correctness of the most recently learned decision tree."
  (format t "~%Estimating the correctness of the current decision tree ...~%")
  (dolist (ex examples)
	  (setf start-time (get-internal-run-time))
	  (when report-progress?
		(format t "~% Considering~%")
		(report-example (get-features ex))
		(format t " with correct classification of ~A ... " 
			(get-classification ex)))
	  (if (equalp (first ex) '+)
	      (incf pos)
	    (incf neg))
	  (if (equalp (first ex) 
		      (make-decision (remove-name-field (get-features ex))))
	      (progn (trace-print report-progress? "correctly classified.~%")
		     (if (equalp (first ex) '+)
			 (incf pos_corr)
		       (incf neg_corr))
		     (incf correct))
	    (trace-print report-progress? "incorrectly classified.~%")))
  (format t "~%Finished after ~,3F seconds per example.~%" 
	  (/ (convert-to-sec (- (get-internal-run-time) start-time)) total))
  (format t "~%~D of ~D positive examples correctly classified, correctness = ~F%~%~%" pos_corr pos (* 100 (/ pos_corr pos)))
  (format t "~%~D of ~D neg examples correctly classified, correctness = ~F%~%~%" neg_corr neg (* 100 (/ neg_corr neg)))
  (format t "~%Correctness on the test set of ~D examples is ~,3F%~%~%" total (* 100 (/ correct total)))
  (/ correct total))

(defun print-decision-tree (&optional (tree *current-tree*) (indent 0))
  "Draw this decision tree, using indentation to indicate levels."
  (cond ((leaf-node? tree) (format t " ~A (~A)" (first tree) (second tree)))
        (t (mapc #'(lambda (sub-tree) 
                      (format t "~%")
                      (dotimes (i (floor (/ indent 3))) (format t "|  "))
                      (format t "~A=~A:" (first tree) (first sub-tree))
                      (print-decision-tree (second sub-tree) (+ indent 3)))
                 (rest tree))))
  nil)

(defun count-interior-nodes (tree)
  "Count the interior (non-leaf) nodes in this tree."
  (if (leaf-node? tree) 0
    (1+ (reduce  #'+ (mapcar #'(lambda (arc) (count-interior-nodes (second arc)))
			     (rest tree))))))

(defun count-leaf-nodes (tree) 
  "Count the leaf nodes in this tree."
  (if (leaf-node? tree) 1
    (reduce #'+ (mapcar #'(lambda (arc) (count-leaf-nodes (second arc))) 
			(rest tree)))))

(defun count-matching-leaves (tree match)
  "Count the number of leaf nodes in this tree that match the second argument."
  (cond ((leaf-node? tree) (if (eq (first tree) match) 1 0))
        ((atom tree) 0)
        (t (+ (count-matching-leaves (first tree) match)
              (count-matching-leaves (rest  tree) match)))))

(defun leaf-node? (x)
  "Determine if this is a leaf node."
  (and (consp x) (member (first x) '(+ - ?))))


