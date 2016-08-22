;;;; UTILS.LISP - a file of utility functions.

;;;; Copyright (c) 1989 by Jude William Shavlik.
;;;;         This program may be freely copied, used, or
;;;;         modified provided that this copyright notice 
;;;;         is included in each copy of this code
;;;;         and parts thereof.

;;;;---------------------------------------------------------------------------
;;;;        Some Useful Functions Specific to Learning Algorithms
;;;;---------------------------------------------------------------------------

;;;  The basic form of unclassified examples will be:
;;;    ( (<attribute1> <value1>) ... (<attributeN> <valueN>) )
;;;  Classified examples will be of the form:
;;;    ( <classification> (<attribute1> <value1>) ... (<attributeN> <valueN>) )

(defvar *domains*          nil 
  "Attributes and their range of possible values - set by user.")
(defvar *all-attributes*   nil 
  "List of known attributes (features) - need not be set by user.")
(defvar *conjuncts-per-line* 3 "Used to save space when printing instances and concepts.")
(defvar *all-features*   nil "List of known features - need not be set by user.")
(defvar *train-examples* nil "Training examples.")
(defvar *test-examples*  nil "Testing examples.")
(defvar *examples*       nil "Classified examples.")
(defvar *train-set-fraction* 0.67  "Used to partition data into two sets.")
(defvar *silent?*              nil "Print NO info on the screen when gathering statistics?")
(defvar *flip-plus-and-minus?* nil "For algorithms biased toward + examples, it is useful
                                    to flip the 'concept' and 'nonconcept' and compare which
                                    is easier to learn.")
(defvar *error-names* nil)
(defvar *convert-ordereds-to-integers?* T "E.g., convert small,med,large to 1,2,3?")

(defun get-classification (example)
  "Get the classification of this classsified  example."
  (first example))

(defun get-features (example)
  "Get the features in this example."
  (if (atom (first example)) ;See if this is a classified example.
      (rest example)
    example))

(defun positive-example? (example)
  "Is this a positive example?"
  (equal '+ (get-classification example)))

(defun negative-example? (example)
  "Is this a negative example?"
  (equal '- (get-classification example)))    

(defun count-positives (examples)
  "Count how many positive examples are in this list."
  (count-if #'positive-example? examples))

(defun get-attribute-values (attribute)
  "Return the possible values of this attribute."
  (rest (assoc attribute *domains*)))

(defun legal-feature-value? (value feature &aux (possibles (get-attribute-values feature)))
  "Is this a legal value for this feature?"
  ;;; Note: this code assumes that only nominal and structured features are being used.
  ;;; Type is specified in *domains*.  Nominals are entered as (feature value1 ... value2)
  ;;; while structured are entered as (feature ISA root-value-name).
  (if (equal 'ISA (first possibles)) ;Check a structured.
      (or (equal value (second possibles))
	  (subclass? value (second possibles) feature))
  (member value possibles))) ;Check a nominal.

(defun collect-examples-with-this-value (examples attribute value)
  "Collect those examples with the given value for the given attribute."
  (remove-if-not #'(lambda (example) 
		     (equalp value (second (assoc attribute (get-features example)))))
		 examples))

(defun build-example-lists (&optional (examples-file "in.data")
				      (remove-name-fields-in-train? t)
				      (remove-name-fields-in-test?  nil)
				      (test-set-fraction 0.67)
			    &aux (examples (permute (read-file examples-file))))
  "Take the examples in this file, mix them up, and divide into training and testing sets.
Unless otherwise specified, the NAME fields are stripped from the training examples but
not from the testing examples."
  ;;; These defaults are chosen because most learning algorithms are not incremental,
  ;;; so reporting the examples during learning is not appropriate.  Testing is inherently
  ;;; incremental, so it makes sense to leave the names available.
  ;;; Note: if a fresh Lisp is started, the same random seed will be used.  Hence, when comparing
  ;;; runs with different algorithms, to insure the same training set, start with a fresh Lisp.
  (when (null examples) (error "No examples!"))
  (let* ((N (length examples))
	 (firstN (round (* test-set-fraction N)))
	 (train (firstN examples firstN))
	 (test (nthcdr firstN examples)))
    (setf *train-examples* (if remove-name-fields-in-train? (remove-name-fields train) train))
    (setf *test-examples*  (if remove-name-fields-in-test?  (remove-name-fields test)  test))))

(defun build-example-lists-for-EBL (&optional (examples-file "CATEGORIZED.DATA")
					      &aux (examples (read-file examples-file)))
  "Take the examples in this file and use the NAME field to label the predicates.
This is needed for DEDUCE to work properly."
  (when (null examples) (error "No examples!"))
  (setf *examples* (mapcar #'(lambda (ex) (let ((name (get-name ex)))
					    ;; Splice name in right after the feature name.
					    (mapcar #'(lambda (feature) 
							(if (listp feature)
							    (cons (first feature) (cons name (rest feature)))
							  feature)) ;Dont change the classification indicator.
						    (remove-name-field ex))))
			   examples)))

(defun report-example (features &aux (count 0))
  "Report the attributes and their values for this example."
  (if (null features)
      (format t "   ()")
    (dolist (feature features)
      (when (and (> count 0) (zerop (mod count *conjuncts-per-line*)))
        (terpri))
      (incf count)
      (format t "~5,10T ~A = ~A" (first feature) (second feature))))
  (terpri))

(defun get-feature-value (example feature)
  "Get this example's value for this feature."
  (second (assoc feature (get-features example))))

(defun validate-examples (examples &aux (no-error? t))
  "See if these examples are complete - all attributes specified - and
consistent - all attributes legal and all values valid.
Report any errors, returning T if there are none."
  (set-all-attributes)
  (dolist (example examples)
    (dolist (feature (get-features example)) ;Check that every feature is legal. 
      (unless (or (equal  (first feature) 'NAME)
		  (member (first feature) *all-attributes*))
	(format t "~%~A in example ~A~% is not a legal attribute.~%"
			   feature example)))
    (dolist (attribute *all-attributes*)  ;Look to see if every attribute specified.
       (let ((value (get-feature-value example attribute)))
         (cond ((null value) 
                  (setf no-error? nil)
                  (format t "~%Attribute ~A not present in example ~A.~%"
			  attribute example))
	       ((legal-feature-value? value attribute) nil)
               (t (setf no-error? nil)
                  (format t "~%~A in example ~A~% is not in the domain of ~A.~%" 
			  value example attribute))))))
   no-error?)

(defun set-all-attributes ()
  "Determine the collection of attributes being used."
  (when (null *domains*) (error "*domains* not set!"))
  (setf *all-attributes* (mapcar #'first *domains*)))

(defun remove-name-fields (examples)
  "Remove the NAME field from these examples.  It is only
there to make the examples human-readable and is not in *DOMAINS*."
  ;;; NAME fields are used by the UNIMEM program via the function ASSIGN-NAMES
  (mapcar #'(lambda (ex) (remove-name-field ex)) examples))

(defun remove-name-field (example)
  "Remove the NAME field from this example."
  (remove-if #'(lambda (feature) (and (listp feature)
				      (equal (first feature) 'NAME)))
	     example))

(defun get-name (example)
  "Get the name of this example."
  (dolist (feature example (error "No name found: ~A" example))
    (if (and (listp feature) (equal 'NAME (first feature))) (return (second feature)))))

(defun assign-names (examples)
  "Look at this list of examples, pulling out the names of the examples
and assigning the example to the name.  That is, evaluating the name
produces the example.  This function returns a list of the names."
  (mapcar #'(lambda (ex) (let ((name (second (assoc 'NAME ex))))
			   (when (null name) (error "No name in ~A." ex))
			   (set name (remove-name-field ex))
			   name))
	  examples))

(defun measure-correctness (checker-function
			    &optional report-progress? (examples *test-examples*)
			    &aux (start-time (get-internal-run-time)) (runtime 0)
			         (correct-pos 0) (correct-neg 0) (total-pos 0) (total-neg 0))
  "Test the correctness of the most recently learned 'classifier'."
  (unless (null examples)
    (trace-print (not *silent?*) "~%Estimating the correctness of the current classifier ...~%")
    (dolist (ex examples)
      (when report-progress?
	(format t "~% Considering example #~D~%" (+ 1 total-pos total-neg))
	(report-example (get-features ex))
	(format t " with correct classification of ~A ... " (get-classification ex)))
      (case (get-classification ex)
	(+ (incf total-pos))
	(- (incf total-neg))
	(error "Bad category: ~A" (get-classification ex)))
      (if (equalp (get-classification ex)
		  (funcall checker-function (remove-name-field (get-features ex))))
	  (progn (trace-print report-progress? "correctly classified.~%")
		 (case (get-classification ex)
		   (+ (incf correct-pos))
		   (- (incf correct-neg))))
	(progn (trace-print report-progress? "incorrectly classified. *****~%")
	       (push (list checker-function (second (assoc 'NAME (get-features ex))))
		     *error-names*))))
    (push '(testing done) *error-names*)
    (setf runtime (convert-to-sec (- (get-internal-run-time) start-time)))
    (unless *silent?*
      (trace-print T
		   "~%Finished after ~,3F seconds (~,3F per example).~%~%" 
		   runtime (/safe runtime (+ total-pos total-neg)))
      (trace-print (and (not *silent?*) (> total-pos 0))
		   "Correctness on the ~3D '+' examples is ~,3F~%" total-pos 
		   (* 100 (/safe correct-pos total-pos)))
      (trace-print (and (not *silent?*) (> total-neg 0))
		   "Correctness on the ~3D '-' examples is ~,3F~%" total-neg 
		   (* 100 (/safe correct-neg total-neg)))
      (trace-print T
		   "Correctness on the test set of ~D examples is ~,3F~%"
		   (+ total-pos total-neg)
		   (* 100 (/safe (+ correct-pos correct-neg) (+ total-pos total-neg))))))
  (values (/safe (+ correct-pos correct-neg) (+ total-pos total-neg))
	  runtime correct-pos correct-neg total-pos total-neg))


;;;;---------------------------------------------------------------------------
;;;;             Functions for Use with ISA Hierarchies
;;;;---------------------------------------------------------------------------

(defun make-hierarchy (net feature)
  "Takes an ISA hierarchy represented as an S-expression and encodes it
as SUBCLASSES and SUPERCLASS property links between classes.
Separate ISA hierarchies share names by using the FEATURE's name. "
  (push (list feature
	      (mapcar #'(lambda (subnet)
			  (push (list feature (first net))
				(get (first subnet) 'SUPERCLASS))
			  (make-hierarchy subnet feature)
			  (first subnet))
		      (rest net)))
	(get (first net) 'SUBCLASSES)))

(defun remove-hierarchy (net feature)
  "Takes an ISA hierarchy represented as an S-expression and removes
the remnants of its encoding on property lists."
  (cond ((null net) nil)
        ((atom net)
	   (setf (get net 'SUBCLASSES) (remove-if #'(lambda (entry) (equal (first entry) feature))
						  (get net 'SUBCLASSES)))  
	   (setf (get net 'SUPERCLASS) (remove-if #'(lambda (entry) (equal (first entry) feature))
						    (get net 'SUPERCLASS))))
	(t (remove-hierarchy (first net) feature)
           (remove-hierarchy (rest  net) feature))))

(defun subclass? (X Y feature &aux (super (get-ISA-link X 'SUPERCLASS feature)))
  "Returns T iff X is a proper subclass of Y in a hierarchy"
  (cond ((null super) nil)
	((equal super Y) t)
	(t (subclass? super Y feature))))

(defun get-ISA-link (item info-name &optional feature)
  "Get the ISA info for this item under this feature."
  (second (assoc feature (get item info-name))))

;;;;---------------------------------------------------------------------------
;;;;             Some Functions used by DEDUCE.LISP
;;;;---------------------------------------------------------------------------

;;; These functions deal with the fact that rule depths may equal the atom LIMIT.

(defun +-with-limit (x y)
  "Add these two numbers, with LIMIT+anything=LIMIT."
  (if (or (eq x 'LIMIT) (eq y 'LIMIT)) 'LIMIT (+ x y)))

(defun max-with-limit (x y)
  "Find the max of these two numbers, with max(LIMIT,anything)=LIMIT."
  (if (or (eq x 'LIMIT) (eq y 'LIMIT)) 'LIMIT (max x y)))

(defun zerop-with-limit (x)
  "Is this a zero? (must handle x=LIMIT)"
  (equal x 0))

(defun <-with-limit (x y)
  "Is x less than y, considering LIMIT as a possible value."
  (cond ((eq x 'LIMIT) nil)
	((eq y 'LIMIT) t)
	(t (< x y))))

;;;;---------------------------------------------------------------------------
;;;;                   Some Miscellaneous Useful Functions
;;;;---------------------------------------------------------------------------

(defun permute (list)
  "Randomize the order of the elements in this list."
  (mapcar #'(lambda (pair) (rest pair))
	  (sort (mapcar #'(lambda (item) (cons (random 1000) item)) list)
		#'(lambda (a b) (> (first a) (first b))))))

(defun choose-random (list)
  "Randomly choose an element in this list."
  (nth (random (length list)) list))

(defun report-time ()
  "Produce a string containing the current time."
  (multiple-value-bind (sec min hour date month year) 
    (decode-universal-time (get-universal-time))
    (format nil "~2D:~2,'0D:~2,'0D ~D/~D/~D" hour min sec month date year)))

(defun convert-to-sec (time)
  "Convert this internal time into seconds."
  (/ time internal-time-units-per-second))

(defun sqr (x)
  "Square this number."
  (* x x))

(defun /safe (x y)
  "Divide safely when x=0=y."
  (cond ((zerop x) 0) ;By assumption.
	(t (/ x y))))

(defun firstN (list N)
  "Return the first N elements of this list."
  (butlast list (max 0 (- (length list) N))))

(defun read-file (file)
  "Read a list from this file, safely closing the file if an error occurs."
  (with-open-file (stream file :direction :input) (read stream)))

(defun tree-member? (item tree &key (test #'equalp))
  "Is this item anywhere within this tree?"
  (cond ((funcall test item tree) t)
	((consp tree) (or (tree-member? item (first tree) :test test)
			  (tree-member? item (rest  tree) :test test)))))
	       
(defun lookup (item a-list &key missing (test #'eq))
  "Return the value associated with item in this assoc-list."
  ;; If item not there, return MISSING (in case there is a need to differentiate from a value of NIL).
  (let ((found-it? (assoc item a-list :test test)))
    (if found-it? (second found-it?) missing)))

;;;;---------------------------------------------------------------------------
;;;;                    Some Miscellaneous Useful Macros
;;;;---------------------------------------------------------------------------

(defmacro trace-print (test-var &rest format-form)
  "Debugging aid for reporting program progress."
  `(if ,test-var (format t ,@format-form)))

(defmacro wait-for-ack (test-var)
  "Pause until user presses ENTER (LF/CR)."
  `(if ,test-var
       (progn (format t "Press <enter> to continue ")
	      (read-char))))

(defmacro report-values (&rest forms)
  "A debugging tool - prints out the value of these forms."
  `(progn
     (format t , (format nil "~%Current Values:~%~A"
			 (for form in forms join (list form '= "~A~%")))
	     ,@ forms)))

(defmacro funcall-if-defined (function &rest args)
  "Call this function, if it is defined."
  `(if ,function (funcall ,function ,@ args)))

(defmacro funcall-else-identity (function &rest args)
  "If function isnt defined, return first arg."
  ;;; Althought wont eval args twice, does copy 1st one twice.
  `(if ,function (funcall ,function ,@ args) ,(first args)))

(defmacro flip (var)
  "Flip (invert) the truth-value of this variable."
  `(setf ,var (not ,var)))

;;;;---------------------------------------------------------------------------
;;;;             Two Functions for Computing Standard Deviations 
;;;;---------------------------------------------------------------------------

(defun compute-std-dev (numbers)
  "Compute the std dev of these numbers."
  (std-dev (apply #'+ (mapcar #'(lambda (i) (sqr i)) numbers))
	   (apply #'+ numbers)
	   (length numbers)))

(defun std-dev (sum-of-squares sum-of-values number-of-values)
  "Determine the standard deviation from these components."
  (sqrt (- (/ sum-of-squares number-of-values) 
	   (sqr (/ sum-of-values number-of-values)))))
