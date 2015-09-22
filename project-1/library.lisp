; defines the class for a basic library-item
(defclass library-item ()
  ((title :initarg :title :reader title)
   (due-date :initform nil :accessor due-date)
   (patron :initform nil :accessor patron)))
   
; defines hte class for book
(defclass book (library-item)
  ((loan-period :allocation :class :initform 14 :reader loan-period)))
  
; defines the class for CD-DVD
(defclass CD-DVD (library-item)
  ((loan-period :allocation :class :initform 5 :reader loan-period)))
  
; defines the basic check-out item
(defmethod check-out ((item library-item) date id)
      (setf (patron item) id))
	  
; defines the check-out method for a book
(defmethod check-out ((item book) date id)
  (if (not (due-date item))
      (progn (setf (due-date item) (+ date (loan-period item))) (call-next-method))))
	  
; defines the check-out method for a CD-DVD
(defmethod check-out ((item CD-DVD) date id)
  (if (not (due-date item))
      (progn (setf (due-date item) (+ date (loan-period item))) (call-next-method))))
	  
; defines the renew method (books only)
(defmethod renew ((item book) date id)
  (if (due-date item)
      (if (<= date (due-date item))
	      (if (equal id (patron item))
		      (progn (setf (due-date item) nil)(check-out item date id))))))
			  
; defines the deposit method.
(defmethod deposit ((item library-item))
  (progn (setf (due-date item) nil) (setf (patron item) nil))
