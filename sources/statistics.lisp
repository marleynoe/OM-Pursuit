(in-package :om)

;%%%%%%%%%% STATISTICAL FUNCTIONS %%%%%%%%%%%

(defun mean (list)
  (/ (om-sum list) (length list))
  )

#|
(defun std-dev (list)
  (let* ((themean (mean list))
         (thedifferences (om- themean list))
         (squaredif (om-square thedifferences))
         (thediffmean (mean squaredif)))
    (sqrt thediffmean)
    ))
|#

(defun std-dev (list)
  (sqrt (mean (om-square (om- (mean list) list)))
    ))

(defun std-dev2 (list)
  (let ((themean (mean list)))
  (* (/ (sqrt (mean (om-square (om- themean list)))) themean) 100)
    ))

(defun variance (list)
  (mean (om-square (om- (mean list) list))
    ))

(defun median-variance (list)
  (mean (om-square (om- (median list) list))
    ))

(defun median-std-dev (list)
  (sqrt (mean (om-square (om- (median list) list)))
    ))

(defun median-std-dev2 (list)
  (let ((themedian (median list)))
    (* (/ (sqrt (mean (om-square (om- themedian list)))) themedian) 100)
    ))

(defun median (list)
  (nth (round (/ (length list) 2)) (sort list '<)))

(defun om-float (input)
  (loop for item in input collect
        (float item)
        ))

(defun om-sqrt (list)
  (mapcar #'(lambda (theitems)
              (sqrt theitems)) list))

(defun om-square (list)
  (mapcar #'(lambda (theitems)
              (expt theitems 2)) list))


(defun covariance (scalars)
  (sqrt (/ (om-sum2 scalars) (length scalars)))
  )
              

(defmethod! om-sum ((self list))
                  (loop for item in self
                  sum item)
                  )
; squared sum
(defmethod! om-sum^2 ((self list))
                  (loop for item in self
                  sum (* item item))
                  )

(defun clip (val &optional (min 0.0) (max 1.0))
" If val is below min, return min,
  if val is above max, return max,
  otherwise return val.
" 
  (let ((from min) (to max))
    (when (> min max) (setf from max) (setf to min))
    (cond
     ((> val to) to)
     ((< val from) from)
     (t val))))

(defmethod! om-clip ((arg1 list) &optional min max)
            (let ((minval (or min (list-min arg1)))
                  (maxval (or max (list-max arg1))))            
                  (mapcar #'(lambda (input)
                               (clip input minval maxval)) arg1))
              )

