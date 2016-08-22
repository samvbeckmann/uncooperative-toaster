;;;*******************************************************************************************
;;;*											     *
;;;*			Minimax and alpha-beta procedures (integrated)                       *	
;;;*											     *
;;;*******************************************************************************************
;;;
;;;*******************************************************************************************
;;;  Top level decision procedure to implement minimax procedure.  If the cutoff
;;; depth is reached, it evaluates the board with static_eval and the number of nodes
;;; evaluated in the current search process is updated.  If the cutoff
;;; depth is not reached, find out if the current board contains a winning or losing
;;; configuration.  If so, return the winning or losing value and the incremented
;;; boards-evaluated.  If not, evaluate the current board by calling either max-value or
;;; min-value depending on whether it is my turn to make a move. Returns a list of three numbers: 
;;; (i) the number of boards evaluated, 
;;; (ii) the slot corresponding to the successor with the best evaluation, and 
;;; (iii) that value. ;;; Don't need this. Never used, not returned.
;;;*******************************************************************************************

(defun minimax-decision (board level alpha-beta? 
			       &optional (my-turn t) (alpha *MINUS-INFINITY*) (beta *PLUS-INFINITY*))
  (let ((result (max-value board t (1- level) alpha-beta? alpha beta 0)))
  	(list (third result) (first result))))

;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are 
;;; evaluated by calling the minimax procedure after decrementing the level and changing the 
;;; player.  This function then returns the maximum evaluation of the successor nodes. 
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, beta is returned, otherwise
;;; alpha is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;        boards-evaluated - number of boards evaluated so far
;;;*******************************************************************************************

(defun max-value (board player level alpha-beta? alpha beta boards-evaluated &optional (slot 0))
	(cond ((not board) (list slot *PLUS-INFINITY* boards-evaluated))

		  ((zerop level) (list -1 (static_eval board) (1+ boards-evaluated)))

		  ((> slot 5) (list slot *MINUS-INFINITY* boards-evaluated))

		  (alpha-beta? (let ((curr (cons slot (rest (min-value (drop-token board slot (if player `black `white))
		  	                                      (not player)
		  	                                      (1- level)
		  	                                      alpha-beta?
		  	                                      alpha
		  	                                      beta
		  	                                      boards-evaluated)))))
		      (setf alpha (max (second curr) alpha))
		      (if (>= alpha beta) (list slot beta (third curr))
		      	  (let ((other (max-value board player level alpha-beta? alpha beta boards-evaluated (1+ slot))))
		      		(if (>= (second curr) (second other))
		          		(list (first curr) (second curr) (+ (third curr) (third other)))
		          		(list (first other) (second other) (+ (third curr) (third other))))))))

		  (t (let ((curr (cons slot (rest (min-value (drop-token board slot (if player `black `white))
		  	                                      (not player)
		  	                                      (1- level)
		  	                                      alpha-beta?
		  	                                      alpha
		  	                                      beta
		  	                                      boards-evaluated))))
		           (other (max-value board player level alpha-beta? alpha beta boards-evaluated (1+ slot))))
		      (if (>= (second curr) (second other))
		          (list (first curr) (second curr) (+ (third curr) (third other)))
		          (list (first other) (second other) (+ (third curr) (third other))))))))


;;;*******************************************************************************************
;;; This function first generates the children of the current board.  If this board has no
;;; children it is evaluated by static_eval and the corresponding value together with incremented
;;; boards-evaluated is returned.  If the input board has one or more children, they are 
;;; evaluated by calling the minimax procedure after decrementing the level and changing the 
;;; player.  This function then returns the minimum evaluation of the successor nodes. 
;;;   If alpha-beta pruning is to be performed, successors are evaluated until alpha
;;; becomes greater than or equal to beta.  If that happens, alpha is returned, otherwise
;;; beta is returned.  In both cases, the number of boards evaluated and the preferred move (slot)
;;; is returned.
;;; INPUT: board - board to be evaluated
;;;        player - t, if computer is to play, nil otherwise
;;;        level - current depth of search
;;;        alpha, beta - pruning parameters
;;;        alpha-beta? - t if alpha-beta pruning is to be performed, nil otherwise
;;;*******************************************************************************************

(defun min-value (board player level alpha-beta? alpha beta boards-evaluated &optional (slot 0))
	(cond ((not board) (list slot *MINUS-INFINITY* boards-evaluated))

		  ((zerop level) (list -1 (static_eval board) (1+ boards-evaluated)))

		  ((> slot 5) (list slot *PLUS-INFINITY* boards-evaluated))

		  (alpha-beta? (let ((curr (cons slot (rest (max-value (drop-token board slot (if player `black `white))
		  	                                      (not player)
		  	                                      (1- level)
		  	                                      alpha-beta?
		  	                                      alpha
		  	                                      beta
		  	                                      boards-evaluated)))))
		      (setf beta (min (second curr) beta))
		      (if (<= beta alpha) (list slot alpha (third curr))
		      	  (let ((other (min-value board player level alpha-beta? alpha beta boards-evaluated (1+ slot))))
		      		(if (<= (second curr) (second other))
		          		(list (first curr) (second curr) (+ (third curr) (third other)))
		          		(list (first other) (second other) (+ (third curr) (third other))))))))

		  (t (let ((curr (cons slot (rest (max-value (drop-token board slot (if player `black `white))
		  	                                      (not player)
		  	                                      (1- level)
		  	                                      alpha-beta?
		  	                                      alpha
		  	                                      beta
		  	                                      boards-evaluated))))
		           (other (min-value board player level alpha-beta? alpha beta boards-evaluated (1+ slot))))
		      (if (<= (second curr) (second other))
		          (list (first curr) (second curr) (+ (third curr) (third other)))
		          (list (first other) (second other) (+ (third curr) (third other))))))))
