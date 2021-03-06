; writes lines in the first file to the second file that contain a string in the third argument
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
	    (progn (format t "Line ~d: ~a~&" num temp) (format outfile "~a~&" temp)))))
	
; tests if one string contains another string, with the option to specify case-sensitive
(defun contains (str test flag &optional (remaintest test))
  (cond
    ((equal remaintest "") T)
	((equal str "") nil)
	((charequal (subseq str 0 1) (subseq remaintest 0 1) flag) (contains (subseq str 1) test flag (subseq remaintest 1)))
	((equal test remaintest) (contains (subseq str 1) test flag test))
	(t (contains str test flag test))))
		  
; Tests if two character are equal, flag for flag sensitive
(defun charequal (char1 char2 flag)
  (if flag (equal char1 char2) (equalp char1 char2)))
