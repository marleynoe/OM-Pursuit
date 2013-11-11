
(in-package :om)

  
(defmethod! mapgrain ((self soundgrain-matrix) matching-fun)
            :icon '(333)
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance 'temporalbox))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om)
                                    (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                          (list (name slot)
                                                (get-array-val self (name slot) col))))))
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (mat-trans (list names vals))))
                    
                    (setf (free-store box) vals)
                    
                    (loop for item in slots do
                          (if (is-om-slot? (type-of box) (car item))
                              
                              (set-slot box  (car item) (if (floatp (cadr item))
                                                            (om-round (cadr item))
                                                          (cadr item)))
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class TemporalBox !" (car item))))
                          )
                    box
                    )))


;(defmethod! grainmap ((self maquette) 

(defmethod! soundgrain-slot (grain name)
            :icon '(335) ;'(333)
            
            :menuins '((1 (("onset" "onset") ("duration" "duration") ("amplitude" "amplitude") 
                            ("file-index" "file-index") ("corpus-index" "corpus-index") 
                            ("iteration-id" "iteration-id") ("filepath" "filepath"))))
            (cadr (find name grain :test 'string-equal :key 'car)))

(defmethod! sgn-slot (grain name)
            :icon '(335) ;'(333)
            
            :menuins '((1 (("onset" "onset") ("duration" "duration") ("magnitude" "magnitude") 
                           ("norm" "norm") ("corpus-index" "corpus-index") ("file-index" "file-index") ("filepath" "filepath"))))
            (cadr (find name grain :test 'string-equal :key 'car)))

; ps how is the color of a temporalbox set? Might be good for symbolic parameters (labels, etc.) or for spatialization data (panning value, etc.)
