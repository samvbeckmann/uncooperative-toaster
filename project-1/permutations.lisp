(defun permutations (arg &aux(result ()))
  (if (equal (length arg) 1) arg
      (dolist (item arg result)
	    (dolist (val (permutations (remove item arg)))
		  (append result (cons item val)))
		(write result))))