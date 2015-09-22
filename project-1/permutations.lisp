; Returns all the permutations of a given list.
(defun permutations (arg &aux(result ()))
  (if (equal (length arg) 1) (list arg)
      (dolist (item arg result)
	    (dolist (val (permutations (remove item arg)))
		  (push (cons item val) result)))))
