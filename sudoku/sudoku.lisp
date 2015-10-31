;;;**************************************
;;; Sodoku Solver (Project 3) by Sam Beckmann
;;;
;;; To use, call the function "sudoku" with the following args:
;;;		- file --> filename of sudoku file, formatted according to your spec.
;;;		- forward-checking? --> t if using forward-checking.
;;;		- arc? --> t if using arc-consistancy.
;;;
;;; Note: Using arc-consistancy without forward checking is not implemented.
;;;
;;;**************************************


;;; Dimention of Sudoku board.
;;; Note: This cannot be changed to use other board sizes.
;;; Other board sizes not implemented.
(defconstant *DIMENSION* 9)

;;; Number of backtracks on current board.
(defvar *backtracks*)

;;; Solves the soduku puzzles in the given file.
;;; Calls run-puzzle on each puzzle in the file.
(defun sudoku (file &optional (forward-checking? nil) (arc? nil) &aux (infile (open file :direction :input
						:if-does-not-exist :error)))
	(let ((temp (read-line infile nil)))
		(dotimes (k (parse-integer temp)) ; loop temp times ;; k is extraneous
			(let ((board (make-array (list *DIMENSION* *DIMENSION*))))
				(dotimes (i *DIMENSION*) ; read puzzle
					(let ((line (read-line infile nil)))
						(dotimes (j *DIMENSION*)
							(setf (aref board i j) (parse-integer (subseq line (* j 2) (1+ (* j 2))))))))
				(run-puzzle board forward-checking? arc?))
			(read-line infile nil))))

;;; Driving function for a single puzzle.
;;; Makes initial call of fill-coord.
(defun run-puzzle (board forward arc)
	(setf *backtracks* 0)
	(format t "Initial Board:~&")
	(print-puzzle board)
	(let ((possible (if (or forward arc) (make-new-possibility-array board) nil)))
		(if forward (progn (format t "~&~%Possibilities after Forward Checking:~&")
			(print-possibilites possible)))
		(if arc (progn (setf possible (arc-consistant board possible))
			(format t "~&~%Possibilities after Arc-Consistancy:~&")
			(print-possibilites possible)))
		(format t "~&~%Solution:~&")
		(if (fill-coord board (find-next-zero board (list 0 0)) 0 forward possible)
			(format t "~&~%I did it!~%~%")
			(format t "~&~%I don't think this one can be solved...~%~%"))
		(format t "Backtracks: ~d~%~%~%~%~%" *backtracks*)))


;;; Attempts to fill the cell at coords with a valid assignment.
;;; Calls itself recursively to solve the entire puzzle.
;;;
;;; Args:
;;;		- board --> current board
;;;		- coords --> coordinates of cell to be filled
;;;		- prev-assign --> the value previously assigned to this cell (non-zero if backtrack)
;;;		- forward --> t if using forward-checking
;;;		- possible --> array of current domains of each variable
(defun fill-coord (board coords prev-assign forward possible)
	(if (null coords)
		(progn (print-puzzle board) t)
		(let ((assignment (assign-next-value
					 	board
					 	coords
					 	prev-assign
					 	forward
					 	(if forward (aref possible (first coords) (second coords)) nil))))
			(if (zerop assignment)
				(progn (incf *backtracks*) (update-board board coords 0) nil)
				(let ((next-zero (find-next-zero board (get-next-coord coords))))
					(if (fill-coord
							(update-board board coords assignment)
							next-zero
							(if (null next-zero) nil (aref board (first next-zero) (second next-zero)))
							forward
							(if forward (remove-possibilities possible assignment coords) nil))
						t
						(fill-coord
							(update-board board coords 0)
							coords
							assignment
							forward
							(if forward (add-possible-back board possible assignment coords) nil))))))))


;;; Prints a given board to the console.
(defun print-puzzle (board)
	(dotimes (i *DIMENSION*)
		(dotimes (j *DIMENSION*)
			(format t "~d " (aref board i j)))
		(format t "~&")))

;;; Prints a given possibility matrix to the console.
(defun print-possibilites (possible)
	(dotimes (i *DIMENSION*)
		(dotimes (j *DIMENSION*)
			(format t "(")
			(dolist (val (aref possible i j))
				(format t "~d " val))
			(format t ") "))
		(format t "~&")))

;;; Given a board and current coordinates, returns the coordinates of the next zero.
;;; If no next zero on board, returns nil.
(defun find-next-zero (board coords)
	(if (null coords)
		nil
		(let ((value (aref board (first coords) (second coords))))
			(if (zerop value)
				coords
				(find-next-zero board (get-next-coord coords))))))

;;; Gets the next coordinate from the board, given current coordinates.
;;; If called repeatedly with the result, will iterate through the whole board.
(defun get-next-coord (coords)
	(cond ((< (second coords) 8) (list (first coords) (1+ (second coords))))
		((< (first coords) 8) (list (1+ (first coords)) 0))
		(t nil)))

;;; Updates a given board at coords with a new assignment.
;;; Returns the reference to the updated board.
(defun update-board (board coords assignment)
	(setf (aref board (first coords) (second coords)) assignment)
	board)

;;; Assigns next valid value to a variable.
;;; If no valid value is possible, returns 0.
;;; Else returns the first valid assignment.
(defun assign-next-value (board coords index forward domains)
	(incf index (if forward (get-inc index (sort domains #'<)) 1))
	(cond ((> index *DIMENSION*) 0)
		((check-constraints board coords index) index)
		(t (assign-next-value board coords index forward domains))))

;;; Gets the increment to the next highest index from a list of domains. Used for forward checking.
;;; If no next domain, returns 10 (greater than range of domains)
(defun get-inc (index domains)
	(cond ((null domains) 10)
		((> (first domains) index) (- (first domains) index))
		(t (get-inc index (rest domains)))))

;; checks if num would still be valid within the constraints, if it was placed at row, col
;; pre-condition: board (row, col) should be equal to 0.
(defun check-constraints (board coords num)
	(if (or (find num (get-board-values board (get-cell-values coords)))
			(find num (get-board-values board (row-enumerator (first coords))))
			(find num (get-board-values board (col-enumerator (second coords)))))
		nil
		t))

;; Returns a list of (x, y) coordinates for a the "cell" of the given row, col
(defun get-cell-values (coords)
	(cond ((and (< (first coords) 3) (< (second coords) 3)) (cell-enumerator 0 0))
		((and (< (first coords) 3) (< (second coords) 6)) (cell-enumerator 0 3))
		((< (first coords) 3) (cell-enumerator 0 6))
		((and (< (first coords) 6) (< (second coords) 3)) (cell-enumerator 3 0))
		((and (< (first coords) 6) (< (second coords) 6)) (cell-enumerator 3 3))
		((< (first coords) 6) (cell-enumerator 3 6))
		((< (second coords) 3) (cell-enumerator 6 0))
		((< (second coords) 6) (cell-enumerator 6 3))
		(t (cell-enumerator 6 6))))

;;; Creates a list of x, y coordinates for a value in a cell
;;; Only takes in the upper-left most value in the cell
(defun cell-enumerator (row col &aux (result (list)))
	(dotimes (x 3 result)
		(dotimes (y 3)
			(push (list (+ row x) (+ y col)) result))))

;;; Creates a list of (x, y) coordinates for a row
(defun row-enumerator (row &aux (result (list)))
	(dotimes (x *DIMENSION* result)
		(push (list row x) result)))

;;; Creates a list of (x, y) coordinates for a column
(defun col-enumerator (col &aux (result (list)))
	(dotimes (x *DIMENSION* result)
		(push (list x col) result)))

;;; Takes in a list of (x, y) pairs, returns a list of the values
;;; at those spots on a given board.
(defun get-board-values (board places &aux (result (list)))
	(dolist (val places result)
		(push (aref board (first val) (second val)) result)))

;;; Creates a new array of possible assignments of variables given a board.
(defun make-new-possibility-array (board &aux (possible (make-array (list *DIMENSION* *DIMENSION*))))
	(dotimes (i *DIMENSION*)
		(dotimes (j *DIMENSION*)
			(setf (aref possible i j) (list 1 2 3 4 5 6 7 8 9))))
	(dotimes (i *DIMENSION*)
		(dotimes (j *DIMENSION*)
			(remove-possibilities possible (aref board i j) (list i j))))
	possible)

;;; Updates the possiblility matrix assuming value is assigned at coords.
(defun remove-possibilities (possible value coords)
	(if (null coords)
		possible
		(progn
			(delete-conflicts possible value (get-cell-values coords))
			(delete-conflicts possible value (row-enumerator (first coords)))
			(delete-conflicts possible value (col-enumerator (second coords)))
			possible)))


;;; Deletes value from the possibily matrix at all values of coord-set
(defun delete-conflicts (possible value coord-set)
	(dolist (coord coord-set)
		(setf (aref possible (first coord) (second coord)) (remove value (aref possible (first coord) (second coord))))))

;;; Adds values back into the possibility matrix assuming that removed-value was removed from coords
(defun add-possible-back (board possible removed-value coords)
	(add-back board possible removed-value (get-cell-values coords))
	(add-back board possible removed-value (row-enumerator (first coords)))
	(add-back board possible removed-value (col-enumerator (second coords)))
	possible)

;;; Adds removed value back into the possible matrix at each place in coord-set if now possible.
(defun add-back (board possible removed-value coord-set)
	(dolist (co coord-set possible)
		(if (check-constraints board co removed-value)
			(setf (aref possible (first co) (second co)) (add-if-not-exist removed-value (aref possible (first co) (second co)))))))

;;; Adds a value to a list if that value is not in the list.
;;; If list is null, returns a new list containing only the given value.
(defun add-if-not-exist (item set)
	(cond ((null set) (list item))
		((find item set) set)
		(t (cons item set))))

;;; **************************
;;; ARC CONSISTANCY ALGORITHMS
;;; **************************

;;; Performs the AC3 algorithm to prune the possibility matrix.
(defun arc-consistant (board possible) 
	(dotimes (i *DIMENSION* possible)
		(dotimes (j *DIMENSION*)
			(if (zerop (aref board i j))
				(check-consistant board possible i j)))))

;;; Checks the consistancy of each value in the domain of the cell at (row, col)
(defun check-consistant (board possible row col)
	(test-domain board possible row col (get-cell-values (list row col)))
	(test-domain board possible row col (row-enumerator row))
	(test-domain board possible row col (col-enumerator col)))

;;; Checks the consistancy of each value in the domain of the cell at (row, col)
;;; against the cells in coord-set.
(defun test-domain (board possible row col coord-set)
	(dolist (co coord-set possible)
		(if (and (not (and (= row (first co)) (= col (second co)))) (zerop (aref board (first co) (second co))))
			(setf (aref possible row col) (consistant (aref possible row col) (aref possible (first co) (second co)))))))

;;; Tests each value in source to see if it is consistant with target.
;;; Calls itself recursively to go through source.
(defun consistant (source target &optional (nsource ()))
	(cond ((null source) nsource)
		((null (remove (first source) target)) (consistant (rest source) target nsource))
		(t (consistant (rest source) target (cons (first source) nsource)))))
