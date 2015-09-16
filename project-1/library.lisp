(defclass library-item ()
  ((title :initarg :title :reader title)
   (due-date :initform nil :accessor due-date)
   (patron :initform nil :accessor patron)))
   
(defclass book (library-item)
  ((loan-period :allocation :class :initform 14 :reader loan-period)))
  
(defclass CD-DVD (library-item)
  ((loan-period :allocation :class :initform 5 :reader loan-period)))
  
(defmethod check-out ((item library-item) date id)
      (setf (patron item) id))
	  
(defmethod check-out ((item book) date id)
  (if (not (due-date item))
      (progn (setf (due-date item) (+ date (loan-period item))) (call-next-method))))
	  
(defmethod check-out ((item CD-DVD) date id)
  (if (not (due-date item))
      (progn (setf (due-date item) (+ date (loan-period item))) (call-next-method))))
	  
(defmethod renew ((item book) date id)
  (if (due-date item)
      (if (<= date (due-date item))
	      (if (equal id (patron item))
		      (progn (setf (due-date item) nil)(check-out item date id))))))
			  
(defmethod deposit ((item library-item))
  (progn (setf (due-date item) nil) (setf (patron item) nil)))