(defun grep-file (file1 file2 string &aux(num 0) &optional flag
				  (infile (open file1 :direction :input
							:if-does-not-exist :error))
				  (outfile (open file2 :direction :output
							:if-does-not-exist :create
							:if-exists :overwrite)))
  (loop
    (setf temp (read-line infile nil 'end-of-file))
	(incf num)
	(when (eq temp 'end-of-file) (close outfile) (close infile) (return))
	(if (contains temp string flag)
	    (progn (format t "Line ~d: ~a~&" num temp) (format outfile "~a~& temp")))))
	
(defun contains (str test flag)
  (cond
    ((equal test nil) T)
	((equal str nil) nil)
	(t (if (charequal (subseq str 0 1) (subseq test 0 1) flag)
	   (if (contains (subseq str 1) (subseq test 1) flag)
	       T
		   (contains (subseq str 1) test flag))
	   (contains (subseq str 1) test flag)))))
		  
; Tests if two character are equal, flag for flag sensitive
(defun charequal (char1 char2 flag)
  (if flag (equal char1 char2) (equalp char1 char2)))
  