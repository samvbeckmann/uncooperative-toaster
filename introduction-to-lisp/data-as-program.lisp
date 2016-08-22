; takes in a single-line function and evaluates it with a random argument between 0 and 1
(defun data-to-program ()
  (format t "Input a one-line function definition:~&")
  (let ((func (read)) (rnd (random 1.0)))
    (eval func)
	(format t "~&When I apply your defined function ~s to the argument ~d I get ~d" (second func) rnd (funcall (second func) rnd))))
