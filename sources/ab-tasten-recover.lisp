; this file is to recover older OMato SDIF files

(in-package :om)

; conversion from SGN-Array to Score-Array doesn't work yet


(defun regroup-frames (glblist sgrlist)
  (flat (loop for glbitem in glblist
        for sgritem in sgrlist collect
        (loop for subsgritem in sgritem collect
              (flat (x-append glbitem subsgritem))
              )
        ) 1)
  )



#|
              (values 
               (length sdiflist)                    ;numatoms             
               (om* reci-fs (first translist))     ;onset (sec)
               (om* reci-fs (second translist))      ;duration (sec)
               (sixth translist)                    ;magnitude (lin)
               (fifth translist)                    ;norm (lin)
               (om-round (third translist))        ;corpus-index (int)
               (om-round (fourth translist))         ;file-index (int)
               (get-sgn-paths self (om-round (third translist)) (om-round (fourth translist))) ;filepath (string)
              )))
|#


;old verions

; ---------------------------

; I need a function to convert an array back into an SDIF file.

(defmethod! sgn-params (&key (markers '((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2))) (corpora t)  
                               (max-atoms 5) (min-deviation 0.001))
                        :icon 04
                        :initvals '(((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2)) nil 5 0.001)
                        :indoc '("markers (sec)" "pathnames/strings pointing to directories containing soundfiles"  "max number of atoms" "min-deviation")
                        (let* ((themarkers (if (sound-p markers)
                                               (markers->frames markers)
                                             markers))
                               (thestring (format nil "\"~a\" '~a' ~d ~d" 
                                                  (python-directories corpora) 
                                                  themarkers
                                                  max-atoms min-deviation)))
                          thestring))
                        

(defmethod! sgn-decomp ((snd sound) (commands string) &key outpath)
            :icon 04
            :numouts 3
            :initvals '(nil nil nil nil 0)
            :indoc '("sound" "parameters" "outpath")
            :outdoc '("decomposition-sdif-file" "decomposition-audio-file" "residual-audio-file")

            (if (probe-file *sgn-path*)
                (let* ((inpath (sound-path snd))
                       (filename (pathname-name inpath))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a_sgn" filename)))
                       (outdir (or outpath (make-pathname :directory (append (pathname-directory inpath) (list name)))))                      
                       (sdif-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                       :name (string+ filename "_soundgrain_decomp") :type "sdif"))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_sg-synthesis") :type "wav"))
                       (residual-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_sg-residual") :type "wav"))
                       )
                  (om-create-directory outdir)
                  (setf str 
                        (format nil "~s ~s ~s ~a" 
                                (namestring *sgn-path*)
                                (namestring inpath)
                                (namestring outdir)
                                commands
                                ))
                  ;(print str)
                  ;(print outdir)
                  ;(print sdif-outfile)
                  ;(print audio-outfile)
                  ;(print residual-outfile)
                  ;(om-cmd-line str *sys-console*)
                  (sys:run-shell-command str :wait nil)
                  (values (probe-file sdif-outfile) (probe-file audio-outfile) (probe-file residual-outfile))           
                  )
              (progn 
                (print "sgnDecomp not found... set in preferences?")
                nil
                ))
            )

(defmethod! get-sgn-params ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 8
            :outdoc '("numatoms" "onset" "duration" "magnitude" "norm" "corpus-index" "file-index" "filepath")
            (let* ((sdiflist (flat (getsdifdata self 0 "XADS" "XSGM" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist))
                   (samplerate (get-decomp-fs self))
                   (reci-fs (/ 1 samplerate)))
              (values 
               (length sdiflist)                    ;numatoms             
               (om* reci-fs (second translist))     ;onset (sec)
               (om* reci-fs (third translist))      ;duration (sec)
               (seventh translist)                  ;magnitude (lin)
               (sixth translist)                    ;norm (lin)
               (om-round (fourth translist))        ;corpus-index (int)
               (om-round (fifth translist))         ;file-index (int)
               (get-soundgrain-paths self (fourth translist) (fifth translist))) ;filepath (string)
               ;(get-sge-paths self (fourth translist) (fifth translist)))
               ))

; (om/ 1 (om-abs (om* thenorm themagnitude)))   ;amplitude (lin)

(defmethod! get-sgn-array ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the sgn array")
            (let* ((thedata (multiple-value-list (get-sgn-params self mintime maxtime)))
                   (thearray (make-instance 'sgn-array 
                                            :numcols (first thedata)
                                            :onset (second thedata)
                                            :duration (third thedata)
                                            :magnitude (fourth thedata)
                                            :norm (fifth thedata)
                                            :corpus-index (sixth thedata)
                                            :file-index (nth 6 thedata)
                                            :filepath (nth 7 thedata)
                                            )))
              thearray))

(defmethod! get-sgn-array-resamp ((self sdiffile) (numcols integer) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled sgn array")
            (let* ((thedata (multiple-value-list (get-sgn-params self mintime maxtime)))
                   (corpus-index (simple-bpf-from-list '(0 1) (sixth thedata) 'bpf 15))
                   (file-index (simple-bpf-from-list '(0 1) (nth 6 thedata) 'bpf 15))
                   (sampled-corpus-index (third (multiple-value-list (om-sample corpus-index numcols))))
                   (sampled-file-index (third (multiple-value-list (om-sample file-index numcols))))
                   (thearray (make-instance 'sgn-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second thedata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third thedata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth thedata) 'bpf 15)   
                                            :norm (simple-bpf-from-list '(0 1) (fifth thedata) 'bpf 15)
                                            :corpus-index corpus-index
                                            :file-index file-index
                                            :filepath (get-soundgrain-paths self sampled-corpus-index sampled-file-index)
                   )))
              thearray))


;make one get-sgn-array in which I can down- or upsample (like plot-sdif2)

(defmethod objfromobjs ((self sdiffile) (type sgn-array))
  (let* ((sdifdata (multiple-value-list (get-sgn-params self)))
         (new (make-instance 'sgn-array 
               :numcols (first sdifdata)
               :onset (second sdifdata)
               :duration (third sdifdata)
               :magnitude (fourth sdifdata)
               :norm (fifth sdifdata)
               :corpus-index (sixth sdifdata)
               :file-index (nth 6 sdifdata)
               :filepath (nth 7 sdifdata)
               )))
    new))

#|
(defun sgn-amplitude (norm magnitude)
  (om/ 1 (om-abs (om* norm (om+ magnitude 0.00000001)))))
|#

(defun sgn-amplitude (norm magnitude)
  (om* (om/ 1 norm) (om-abs magnitude)))

;-------------

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

;------

(defun get-midicent (string)
  (string-to-number (my-string-until-char (second (multiple-value-list (my-string-until-char string "_"))) "-")))

(defun get-velocity (string)
  (string-to-number (my-string-until-char (second (multiple-value-list (my-string-until-char string "-"))) "_")))

#|
; throws an error for some reason

(defmethod* PrepareToPlay ((player t) (self note) at &key approx port interval voice)
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
|#

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


