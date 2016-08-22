; returns the row in the given column that has the least number of conflicts with the current board state.
(defun find-min-conflicts (board col endRow &optional (startRow 0))
  (if (>= startRow endRow)
      endRow
	  (let ((min-rest (find-min-conflicts board col endRow (1+ startRow))))
	       (if (< (check-conflicts board (list col startRow)) (check-conflicts board (list col min-rest)))
	        startRow
		    min-rest))))

; returns the pair of the queen in the given column, if one exists
(defun get-queen-in-col (board col)
  (cond ((<= (length board) 0) nil)
        ((equal (first (first board)) col) (first board))
		(t (get-queen-in-col (rest board) col))))

; returns the number of conflicts a position has on the board
(defun check-conflicts (board position &aux(counter 0))
  (dolist (queen board counter)
    (if (is-conflict queen position) (incf counter) ())))

; tests if there is a conflict between two positions
(defun is-conflict (pos1 pos2)
  (cond ((equal pos1 pos2) nil)
        ((= (first pos1) (first pos2)) t)
        ((= (second pos1) (second pos2)) t)
		((= (abs (- (first pos1) (first pos2))) (abs (- (second pos1) (second pos2)))) t)
		(t nil)))

; finds the queen on the board that currently has the max # of conflicts
(defun find-max-conflicts (board endcol &optional (startcol 0))
  (if (>= startcol endcol) endcol
      (if (> (check-conflicts board (get-queen-in-col board startcol)) (check-conflicts board (get-queen-in-col board (find-max-conflicts board endcol (1+ startcol)))))
	      startcol
		  (find-max-conflicts board endcol (incf startcol)))))

; gets the col of a queen from the set of conflicted queens.
(defun get-conflicted-queen (board)
  (loop
    (let ((col (random (length board))))
	  (if (> (check-conflicts board (get-queen-in-col board col)) 0)
	      (return col)))))
  

; removes the current queen in the given col, replacing it with one in the newrow arg.		
(defun update-queen (board col newrow)
  (dolist (queen board)
    (if (= (first queen) col)
	    (return (cons (list col newrow) (remove queen board))))))
		
; makes the initial random assignments for the board		
(defun assign-board (n &optional (x 0)(board ()))
  (if (>= x n) board
      (assign-board n (1+ x) (cons (list x (random n)) board))))

; checks to see if the board is solved in its current state  
(defun no-conflicts (board &aux (conflicts 0))
  (dolist (queen board conflicts)
    (if (unless (zerop (check-conflicts board queen)))
	    (incf conflicts))))

; performs a single step of the nqueens program.
(defun unit-step (board n steps-remain)
  (cond ((<= steps-remain 0) nil)
        ((zerop (no-conflicts board)) board)
		(t (let ((col (get-conflicted-queen board)))
		    (unit-step (update-queen board col (find-min-conflicts (remove (get-queen-in-col board col) board) col (1- n))) n (decf steps-remain))))))

; fucntion that drives the nqueens program
(defun nqueens(n maxsteps)
  (unit-step (assign-board n) n maxsteps))
  
; Note on output:
; an output of nil means no solutions was found.
; an output of of a list is the result. The first item in each pair is the column of a queen, the next number is a row. The list is zero-indexed.
