;; This file contains parts of the code needed to do the learning program problem
;; in your first project

(defun read-train-file (f1 &aux temp (infile (open f1 :direction :input :if-does-not-exist :error)))
" Function to read in training data"

  (loop 
   (let ((next (read infile nil 'end-of-file)))
     (when (eq next 'end-of-file)
	   (close infile) 
	   (return temp))
     (push (list next
		 (read infile nil 'end-of-file)
		 (read infile nil 'end-of-file)
		 (read infile nil 'end-of-file))
	   temp))))

(defun read-test-file (f1 &aux temp (infile (open f1 :direction :input :if-does-not-exist :error)))
" Function to read in test data"
  (loop 
   (let ((next (read infile nil 'end-of-file)))
     (when (eq next 'end-of-file)
	   (close infile)
	   (return temp))
     (push (list next
		 (read infile nil 'end-of-file)
		 (read infile nil 'end-of-file))
	   temp))))

; finds the k closest neighbors to the "new" point
(defun find-neighbors (k new stored) ; returns a list of the k nearest neighbors
  (if (<= k 1) (list (findmin new stored))
      (cons (findmin new stored) (find-neighbors (decf k) new (remove (findmin new stored) stored)))))

; finds the closest point from a train-set to a given point
(defun findmin (point train-set &optional closest)
  (cond ((not train-set) closest)
        ((not closest) (findmin point (rest train-set) (first train-set)))
		(t (findmin point (rest train-set) (compare-point point closest (first train-set))))))

; compares two options and returns the one closer to the test point
(defun compare-point (point option1 option2)
  (if (<= (get-distance point option1) (get-distance point option2)) option1 option2))

; returns the distance bewteen two points
(defun get-distance (point testpoint)
  (+ (abs (- (first point) (second testpoint))) (abs (- (second point) (third testpoint)))))
  
; prints a given list of points
(defun print-points (neighbors test)
  (dolist (neigh neighbors)
    (if (= (first neigh) test) (format t "~s~&" neigh))))
    
(defun nearest-neighbor ()
"Driver function for the learning program"

  (format t "Input training file name: ")
  (let ((train-set (read-train-file (read))))
    (format t "Read ~d training instances~&~%" (length train-set))
    (format t "Input test file name: ")
    (let ((test-set (read-test-file (read))))
      (format t "Read ~d test instances~&~%" (length test-set))
      (format t "~&Input value of k for calculating k-nearest-neighbors: ")
      (let ((k (read)))
	(dolist (triplet test-set (format t "~&~%!!!!Aint I a good learner??!!"))
		(let ((neighbors (find-neighbors k triplet train-set)))
		  (format t "*************************************~&")
		  (let ((ones 0) (zeroes 0))
		    (dolist (neigh neighbors)
			  (if (= (first neigh) 1) (incf ones) (incf zeroes)))
			(format t "The likely classification of ~s is ~d because of the ~d closest stored points the following ~d are of class ~d:~&"
					  triplet (if (> ones zeroes) 1 0) k (max ones zeroes) (if (> ones zeroes) 1 0))
			(print-points neighbors (if (> ones zeroes) 1 0))
			(format t "Other points belonging to class ~d:~&" (if (> ones zeroes) 0 1))
			(print-points neighbors (if (> ones zeroes) 0 1)))))))))
