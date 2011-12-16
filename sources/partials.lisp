(in-package :om)

(defmethod! get-partial-params ((self sdiffile))
            :icon 04
            :outdoc '("numpartials" "onset" "duration" "magnitude" "frequency" "phase")
            :numouts 8
            (let ((sdifpartials (getsdifpartials self)))
              (setf numcols (length sdifpartials))
              (setf partialdata (mat-trans sdifpartials)))
            (values
             numcols                 ;numatoms 
             (second partialdata)    ;onset (sec)
             (third partialdata)     ;duration (sec)
             (fourth partialdata)    ;magnitude (lin)
             1                       ;norm (lin) -could later calculate it from phase/mag/dur
             (first partialdata)     ;frequency (Hz)
             (fifth partialdata)     ;phase (rad)
             0                       ;bandwidth
             ))


(defmethod! get-partial-array-resamp ((self sdiffile) (numcols integer))
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled partial-array")
            (let* ((partialdata (multiple-value-list (get-partial-params self)))
                   (thearray (make-instance 'partial-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second partialdata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third partialdata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth partialdata) 'bpf 15)
                                            :norm 1
                                            :frequency (simple-bpf-from-list '(0 1) (sixth partialdata) 'bpf 15)
                                            :phi (simple-bpf-from-list '(0 1) (seventh partialdata) 'bpf 15)
                                            :bandwidth 0
                                            )))
              thearray))

(defmethod objfromobjs ((self sdiffile) (type partial-array))
  (let* ((partialdata (multiple-value-list (get-partial-params self)))
         (new (make-instance 'partial-array 
                             :numcols (first partialdata)
                             :onset (second partialdata)
                             :duration (third partialdata)
                             :magnitude (fourth partialdata)
                             :norm 1
                             :frequency (sixth partialdata)
                             :phi (seventh partialdata)
                             :bandwidth 0
                             )))
    new))

;; %%%%%%%%% helper function %%%%%%%%%%%%%

(defmethod! getsdifpartials ((self sdiffile))
   :indoc '("an SDIF file")
   :doc "Returns a list of partial data from an sdif file (using 1MRK / 1TRC frames).

Output is a list of  sublists of (freq [Hz] onset [s]  duration [s]  magnitude [lin]  phase [radians]).
"
   :icon 639
   (let ((rawdata (chord-seq-raw-data self 'all)))
     (mapcar #'(lambda (partial)
                 (if (consp (car partial))
                     ;;; trc
                     (let ((t1 (list-min (nth 0 partial))) 
                           (t2 (list-max (nth 0 partial))))
                       (list (om-mean (nth 1 partial)) 
                             t1 (- t2 t1)
                             (om-mean (nth 2 partial))
                             (om-mean (nth 3 partial))))
                   ;;; mrk
                   (list   (- (nth 3 partial) (nth 0 partial)) (nth 2 partial) (nth 1 partial) (nth 4 partial) (nth 0 partial))
                   ))
             rawdata)
     ))

