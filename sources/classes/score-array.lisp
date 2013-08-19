(in-package :om)


;; this should be mainly regarded like a synthesis class and I need objfromobj methods to convert from the synthesizers to this...
(defclass! score-array  (class-array)
           (
            (midicent  :accessor midicent  :initarg :midicent  :initform 6000)
            (onset     :accessor onset     :initarg :onset     :initform 0)
            (duration  :accessor duration  :initarg :duration  :initform 1000)           
            (velocity  :accessor velocity :initarg :velocity :initform 100)
            (offset  :accessor offset :initarg :offset :initform 0)
            (channel  :accessor channel :initarg :channel :initform 1)
            (legato  :accessor legato :initarg :legato :initform 0)
            (chord :accessor chord :initarg :chord :initform nil))
           (:icon 138)
           )

; %%%%%%% CHORDSEQ 2 VOICE %%%%%%%%%%

(defmethod! chordseq->voice ((self chord-seq) (tempi t) (measures list) (max-subdivision t) (ties t) (true-durations t) &optional (forbidden-subdivision nil) (grace-notes 0) (precision 0.5))
            :icon '(138)
            :initvals '(nil 60 '(4 4) 8 nil nil nil 0 0.5)
            (let* ((chordseq self)
                   (thechords (chords chordseq))
                   (thedurs (if (equal true-durations 1)
                                (true-durations chordseq)
                              (ldur chordseq)));)
                   (thetree (omquantify (flat thedurs) tempi measures max-subdivision forbidden-subdivision grace-notes precision))
                   (thevoice (make-instance 'voice
                                            :tree thetree
                                            :chords thechords
                                            :tempo tempi
                                            :legato (legato chordseq)
                                            :ties ties
                                            )))
              thevoice))


              

(defmethod objfromobjs ((self chord-seq) (type voice))
  (let* ((thevoice (chordseq->voice self 60 '(4 4) 8 nil nil '(0) 0.5)))
    thevoice))

(defmethod objfromobjs ((self chord-seq) (type multi-seq))
  (let* ((themultiseq (micro->multi self 10)))
    themultiseq))
         

; %%%%%%% GABOR %%%%%%%%%%

(defmethod objfromobjs ((self gabor-array) (type score-array))
  (let* ((gabordata (data self))
         (new (make-instance 'score-array
                             :numcols (length (first gabordata))
                             :midicent (f->mc (fifth gabordata))
                             :onset (om-round (om* 1000 (first gabordata)))
                             :duration (om-round (om* 1000 (second gabordata)))
                             :velocity (om-scale (gabor-amplitude (fourth gabordata) (third gabordata)) 1 127) ;this doesn't consider bandwidth... (lin->db 
                             :chord (nth 7 gabordata))))
    new))


; %%%%%%%%%% FOF %%%%%%%%%%%%

(defmethod objfromobjs ((self fof-array) (type score-array))
  (let* ((fofdata (data self))
         (new (make-instance 'score-array
                             :numcols (length (first fofdata))
                             :midicent (f->mc (fifth fofdata))
                             :onset (om-round (om* 1000 (first fofdata)))
                             :duration (om-round (om* 1000 (second fofdata)))
                             :velocity (om-scale (lin->db (fof-amplitude (fourth fofdata) (third fofdata))) 1 127) ;this doesn't consider bandwidth nor skirtwidth...
                             :chord (nth 10 fofdata))))
    new))

; %%%%%%%%%%%% PARTIALS %%%%%%%%%%%%%%

(defmethod objfromobjs ((self partial-array) (type score-array))
  (let* ((partialdata (data self))
         (new (make-instance 'score-array
                             :numcols (length (first partialdata))
                             :midicent (f->mc (fifth partialdata))
                             :onset (om-round (om* 1000 (first partialdata)))
                             :duration (om-round (om* 1000 (second partialdata)))
                             :velocity (om-scale (lin->db (third partialdata)) 1 127) ;this doesn't consider bandwidth nor skirtwidth...
                             :chord (nth 10 partialdata))))
    new))

; %%%%%%%%%%%%% SOUNDGRAIN LABELLED %%%%%%%%%%%%%%%

;perhaps map the corpus-index to MIDI-channel? or simply include the additional params as keyword slots. Yes! Up to 16 Corpora then :-)

;moved this temporarily into sgl-array to avoid startup-errors when loading the lib (as 'sgl-array' isn't defined yet)

#|

(defmethod objfromobjs ((self sgl-array) (type score-array))
  (let* ((sgldata (data self))
         (new (make-instance 'score-array
                             :numcols (length (first sgldata))
                             :midicent (nth 7 sgldata)
                             :onset (om-round (om* 1000 (first sgldata)))
                             :duration (om-round (om* 1000 (second sgldata)))
                             :velocity (nth 8 sgldata) ;this doesn't consider bandwidth nor skirtwidth...
                             :chord (nth 9 sgldata))))
    new))

|#

;these should later be my special score-objs (with starttime)
;I should probably change this so it groups notes into chords already! (molecules=chords)

(defmethod objfromobjs ((self score-array) (type chord-seq))
  (let* ((scoredata (data self))
         (new (make-instance 'chord-seq 
                             :lmidic (first scoredata)
                             :lonset (om-round (second scoredata))
                             :ldur (om-clip (om-round (third scoredata)) 1 nil)
                             :lvel (om-abs (fourth scoredata))
                             :lchan (sixth scoredata))))
    new))


;note: this should correctly be done with loops that consider the lists of lists inside etc... this assumes only 1-note-chords
(defmethod objfromobjs ((self chord-seq) (type score-array))
  (let* ((new (make-instance 'score-array 
                             :numcols (length (lmidic self))

                             :midicent (flat (lmidic self))

                             :onset (lonset self)
                             :duration (flat (ldur self))
                             :velocity (flat (lvel self))
                             :offset (flat (loffset self))
                             :channel (flat (lchan self))
                             :legato (legato self))))
    new))

(defmethod objfromobjs ((self voice) (type score-array))
  (Objfromobjs (Objfromobjs self (mki 'chord-seq)) (mki 'score-array)))

(defmethod! micro->multi ((score voice) (approx integer) &key port-list channel-list)
            (micro->multi (ObjfromObjs score (mki 'chord-seq)) approx :port-list port-list :channel-list channel-list))


; %%%%%%%%%%%%% DIRECTLY FROM PARTIALS %%%%%%%%%%%%%%%%%%%

; how do I distinguish objfromobj with different SDIFs (Fof, Gabor, partial-tracking, etc.)?
;maybe here a cond-statement for the different sdifs

(defmethod! get-score-params ((self sdiffile))
            :icon 138
            :numouts 6
            (when (equal (find-in-nvtlist (getnvtlist self) "TableName") "SinusoidalTracks")  ;check if it's a partial-tracking SDIF
              (let ((sdifchords (getsdifchords self)))
                (setf numcols (length sdifchords))
                (setf scoredata (x-append (mat-trans sdifchords) nil)))
              )
            (values
             numcols
             (f->mc (first scoredata))
             (om-round (om* 1000 (second scoredata)))
             (om-round (om* 1000 (third scoredata)))
             (om-scale (lin->db (fourth scoredata)) 1 127)
             (fifth scoredata)
             ))


; from sinusoidal tracks: 
; should have an 'if-case' to check

(defmethod objfromobjs ((self sdiffile) (type score-array))
  (let* ((scoredata (multiple-value-list (get-score-params self)))
         (new (make-instance 'score-array 
                             :numcols (first scoredata)
                             :midicent (second scoredata)
                             :onset (third scoredata)
                             :duration (fourth scoredata)
                             :velocity (fifth scoredata)
                             :chord (sixth scoredata))))
    new))