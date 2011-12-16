(in-package :om)



(defmethod! dispatch-chord-seq ((cseq chord-seq) (approx integer) &key port-list channel-list)
            (let* ((nvoices (/ approx 2))
                   (min-div (/ 200 approx)))
              (loop for i from 0 to (1- nvoices) collect
                    (let ((cs (clone cseq)))
                      (loop for chord in (inside cs) do
                            (loop for note in (inside chord) do
                                  (let* ((approx-pitch (* min-div (round (midic note) min-div)))
                                         (cs-num (round (mod approx-pitch 100) min-div)))
                                    (unless (= i cs-num)
                                      (setf (inside chord) (remove note (inside chord))))))
                            (unless (inside chord)
                              (setf (inside cs) (remove chord (inside cs)))))
                      (adjust-extent cs)
                      (QNormalize cs)
                      (set-port cs (or (nth i port-list) 0))
                      (set-channel cs (or (nth i channel-list) 1))
                      cs))
              ))

(defun micro-channel (midic approx)
  (+ 1 (/ (mod midic 100) (/ 200 approx))))


(defmethod* PrepareToPlay ((player t) (self note) at &key  approx port interval voice)
   (when (and *midiplayer* (not (memq (tie self) '(continue end))))
     (setf port (or port (port self)))
     (let ((chan (+ (1- (chan self))  (1- (micro-channel (approx-m  (midic self) approx) approx))))
           (pitch (truncate (approx-scale (get-current-scale approx) (midic self)) 100))
           (vel (vel self))
           (dur (- (real-dur self) 2))
           (date (+ *MidiShare-start-time* at))
           (voice (or voice 0)))
       (if interval
         (let ((newinterval (interval-intersec interval (list at (+ at (- (real-dur self) 1)))))) 
           (when newinterval
             (playnote port chan pitch vel (- (second newinterval) (first newinterval) 1) 
                       (- (+  *MidiShare-start-time* (first newinterval)) 
                          (first interval))
                       voice)))
         (playnote port chan pitch vel dur date voice)))))

(defmethod! micro->multi ((score chord-seq) (approx integer) &key port-list channel-list)
            :icon '(141)
            (let* ((chordseqlist (dispatch-chord-seq score approx :port-list port-list :channel-list channel-list))
                   (micromulti (make-instance 'multi-seq
                                              :chord-seqs chordseqlist)))
              micromulti))

(defmethod! micro->multi ((score voice) (approx integer) &key port-list channel-list)
            (micro->multi (ObjfromObjs score (mki 'chord-seq)) approx :port-list port-list :channel-list channel-list))

(defmethod! micro->multi ((score list) (approx integer) &key port-list channel-list)
            (mapcar (lambda (thelist)
                      (micro-chordseq->multiseq thelist approx :port-list port-list :channel-list channel-list)) score))
