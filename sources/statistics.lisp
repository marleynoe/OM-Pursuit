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
(defmethod! om-sum2 ((self list))
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

;##### DESCRIPTORS ########

(defmethod! sox-centroid ((freqs list) (amps list))
           (/
            (loop for x in freqs 
                  for y in amps
                  finally
                  sum (* x y)
                  )
            (+ (om-sum amps) 0.00000001) ;avoid division by zero
           ))


(defmethod! sox-spectral-spread ((freqs list) (amps list) (centroid number))
           (/
            (loop for x in freqs 
                  for y in amps
                  finally
                  sum (* (* (- x centroid) (- x centroid)) y)
                  )
            (+ (om-sum amps) 0.00000001) ;avoid division by zero
           ))

(defmethod! sox-spectral-decrease ((freqs list) (amps list))
           (let ((a1 (car amps)))
            (/
            (loop for f in freqs 
                  for a in amps
                  finally
                  sum (- a a1)
                  )
            (om+ (loop for a in amps
                  for b in (cdr amps)
                  finally
                  sum (* a b)
                  )
                 0.00000001) ; avoid division by zero
           )))

(defmethod! sox-energy ((freqs list) (amps list))
           (/
            (loop for x in amps
                  finally
                  sum (* x x)
                  )
            (+ (om-sum amps) 0.00000001) ;avoid division by zero
           ))

(defmethod! sox-spectral-descriptor ((descriptor string) (freqs list) (amps list))
           ; :menuins '(())
            (cond ((equal descriptor "SpectralCentroid") (sox-centroid freqs amps))
                  ((equal descriptor "SpectralSpread") (sox-spectral-spread freqs amps (sox-centroid freqs amps)))
                  ((equal descriptor "SpectralDecrease") (sox-spectral-decrease freqs amps))
                  ))

; this function can also be useful for the constraint-building function (menu for the choice of the descriptor)
(defun sox-descriptor-to-type (descriptor)
  (cond ((equal descriptor "SpectralCentroid") "1SCN")
        ((equal descriptor "SpectralSpread") "1SSP")
        ((equal descriptor "SpectralDecrease") "1SPD")
        )
  )
; should be simply a menu with the 'long word' that gets converted into the 4ascii-characters


(defvar *sox-descriptors* '("SpectralCentroid" "SpectralSpread" "SpectralDecrease"))

