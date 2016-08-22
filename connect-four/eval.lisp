;;;********************************************************************************************
;;;*											      *
;;;*			Board Evaluation Routines					      *
;;;*											      *
;;;********************************************************************************************
;;;
;;; WINNING is a very high value returned when a winning configuration is detected.
;;; LOSING is a very low value returned when a losing configuration is detected.
;;;
(defvar *WINNING* (* 10 *WIN*))
(defvar *LOSING*  (* 10 *LOSE*))

;;*********************************************************************************************
;; This is the board-evaluator function called from minimax to evaluate a board.It returns an
;; integral value that reflect the promise of the input board.
;; INPUT: Current board
;;*********************************************************************************************
(defun static_eval (board)
  (rank_board (convert_to_array board)))

;;**********************************************************************************************
;; The following function convert an input board to array representation.  It
;; is this array representation that is passed by the function static_eval() to 
;; the function rank_board() 
;; INPUT: current board.
;;*********************************************************************************************
(defun convert_to_array (board)
  (let ((index 0)
	(squares (make-array (* *Dimension* *Dimension*))))
    (dotimes (i *Dimension* squares)
	     (dotimes (j *Dimension*)
		      (setf (aref squares index)
			    (aref board i j))
		      (incf index)))))

;;**********************************************************************************************
;; This function, on being given the array representation of a board returns an integer that is
;; the rank of the board from the view-point of the maximising player. Note that the variable
;; win_positions is a list of all possible winning configurations.
;; INPUT: array representation of current board.
;;*********************************************************************************************
(defun rank_board (board_array &aux (count 0))
	(dolist (position *WIN_POSITIONS*)
		(let ((black 0) (white 0))
			(dolist (spot position)
				(cond ((equal (aref board_array spot) 'black) (incf black))
				      ((equal (aref board_array spot) 'white) (incf white))))
			(cond ((= black 4) (incf count *WINNING*))
				  ((= white 4) (incf count *LOSING*))
				  ((= black 3) (if (zerop white) (incf count 10)))
				  ((= black 2) (if (zerop white) (incf count 2)))
				  ((= black 1) (if (zerop white) (incf count 1)))
				  ((= white 3) (decf count 10))
				  ((= white 2) (decf count 5))
				  ((= white 1) (decf count 1))
				  )))
	count)
