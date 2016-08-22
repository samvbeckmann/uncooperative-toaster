; Take the newton root of the first argument with the accuracy of the next argument
(defun newt-root (number tol &optional(x 1))
  (if (< (abs (- (expt x 4) number)) tol)
       (float x)
	   (newt-root number tol (float (- x (/ (- (expt x 4) number) (* 4 (expt x 3))))))))
