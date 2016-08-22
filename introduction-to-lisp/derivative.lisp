; performs symbolic differentiation on a given function
(defun deriv (arg var)
  (cond ((atom arg) (if (equal arg var) 1 0)) ; constant u
        ((equal (first arg) '+) `(+ ,(deriv (second arg) var) ,(deriv (third arg) var))) ; addition
		((equal (first arg) '-) `(- ,(deriv (second arg) var) (deriv (third arg) var))) ; subtraction
		((equal (first arg) '*) `(+ (* ,(second arg) ,(deriv (third arg) var)) (* ,(third arg) ,(deriv (second arg) var)))) ; multiplication
		((equal (first arg) '/) `(/ (- (* ,(third arg) ,(deriv (second arg) var)) (* ,(second arg) ,(deriv (third arg) var))) (expt ,(third arg) 2))) ; divison
		((equal (first arg) 'exp) `(* (exp ,(second arg)) ,(deriv (second arg) var))) ; exponential
		((equal (first arg) 'expt) `(* ,(third arg) (expt ,(second arg) ,(- (third arg) 1)) ,(deriv (second arg) var))) ; expt
  )
)
