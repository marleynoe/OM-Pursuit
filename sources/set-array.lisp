(in-package :om)




; again arrays %%%%%%%%%%%%%%%%%%%%%%%%%

(defun set-array (type numcols params)
 (let ((array (cons-array (make-instance type) (list nil numcols 0 nil) params)))
   (set-data array)
   array)
 )

(defun give-array (theparameters)
  (print theparameters)
  (set-array 'add-1 (length (car theparameters)) (list :e-dels (car theparameters) 
                                                       :amp (second theparameters)
                                                       :durs (third theparameters))
             ))


; %%%%%%%%%%%%%%%%%%%%%%%
#|

(defmethod! gen-array ((vals list) (cols number))
(let ((array (cons-array (make-instance 'add-1
                                        :freq '(1 2 3 4 5))
                         (list nil cols 0 '((1 2 3 4 5))) 
                         '(:test-keyword (4 5 6 7 6 5 4)
                           :test-anotherone (2 3 4 5 6 7)))))
  (set-data array)
  (setf (e-dels array) (first vals))
  (setf (durs array) (second vals))
  array)
)



(defmethod initialize-instance :after ((self myclass) &rest initargs)
	(declare (ignore initargs))
	(setf (my-hidden-slot self) (myfunction (visible-slot-1 self) (visible-slot-2 self)))
	self)

;doesn't work
(defun set-array (thevals)
  (let ((array (cons-array (make-instance 'add-1
                                          :numcols 5
                                          :e-dels '(0 1 2 3 4)) '(nil 7 0 nil) '(:test-keyword thevals))))
       (set-data array)
       array)
  )

; this seems to work if it wasn't for the quotes
(defun set-array2 (thetype theparams)
  (let ((array (cons-array (make-instance thetype)
                           '(nil 7 0 nil)  theparams))))
    (set-data array)
    array)
  )

; anohter try 
(defun set-array3 (thetype numcols theparams)
  (let ((array (cons-array (make-instance thetype)
                           (quote (nil numcols 0 nil)) theparams)))
    (set-data array)
    array)
  )

(set-array2 'add-1 '(:e-dels (0 3 1 3 4) 
                     :durs (0 5 3 5 4) 
                     :test-keyword (4 5 6 5 6)))

(set-array3 'add-1 5 '(:e-dels (0 3 1 3 4) 
                       :durs (0 5 3 5 4) 
                       :test-keyword (4 5 6 5 6)))

; this seems to work if it wasn't for the quotes
(defun set-array4 (thetype numcols theparams)
  (let* ((thelist (list nil numcols 0 nil))
        (array (cons-array (make-instance thetype)
                           (quote thelist) theparams)))
    (set-data array)
    array)
  )

|#
