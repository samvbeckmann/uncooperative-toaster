; returns the row in the given column that has the least number of conflicts with the current board state. TODO: Testing
(defun findMinConflicts (board col, endRow, startRow)
  (if (>= startRow endRow)
      endRow
	  (if (< (checkConflicts board (newPair col startRow)) (checkConflicts board col endRow (findBig board col endRow (1+ startRow))))
	      startRow
		  (findBig board col endRow (1+ startRow)))))
		  
; checks to see if the board is solved in its current state TODO: Testing
(defun checkSolved (board n &optional (col 1))
  (cond ((>= col n)
            T)
		((equal 0 (checkConflicts board (getQueenInCol board col)))
		    (checkSolved board n (1+ col)))
	    (t nil)))
		
; needed functions
; - getQueenInCol (board col): returns of pair that contains the queen in the given column, if one exists.
; - checkConflicts(board position): returns the number of conflicts a queen in a given position has
; - nqueens(n): Runs the nqueens program
; - displayResults(board): Prints the state of a completed board to the console.
; - assignBoard(n): creates a new board from scratch

(defun nqueens(n )
  )
  
(defun getQueenInCol (board col)
  )