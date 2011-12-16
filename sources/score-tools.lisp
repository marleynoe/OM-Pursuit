(in-package :om)


;; I should do sth similar when I make chord-seqs from my note-arrays!

(defmethod! partials->chord-seq ((self sdiffile) &key (minvel 1) (maxvel 127) (expt 1.0))
   :indoc '("a partial-tracking SDIF file")
   :initvals '(nil 1 127 1)
   :doc "Generates a CHORD-SEQ instance from the 1TRC or 1MRK frame data in <self>.

Internally calls and formats data from GetSDIFChords.
"
   :icon 639
   (print minvel)
   (let* ((rawdata (sort (GetSDIFChords self) '< :key 'cadr))
          (chords nil) (cseqdata nil))
     (loop for note in rawdata do
           ;;; note = (pitch onset dur vel)
           ;;; (car chords) = (onset (pitches) (durs) (vels)) 
           (if (and (car chords) 
                    (= (second note) (car (car chords))))
               ;;; add note to chord
               (setf (car chords)
                     (list (first (car chords))
                           (append (second (car chords)) (list (first note)))
                           (append (third (car chords)) (list (third note)))
                           (append (fourth (car chords)) (list (fourth note)))))
             ;;; else create new chord
             (push (list (second note) (list (first note)) (list (third note)) (list (fourth note)))
                   chords)))
     (setf cseqdata (mat-trans chords))
     (make-instance 'chord-seq
                    :lonset (om-round (om* (first cseqdata) 1000))
                    :lmidic (om-round (f->mc (second cseqdata)))
                    :ldur (om-round (om* (third cseqdata) 1000))
                    :lvel (om-round (om-scale-exp (fourth cseqdata) minvel maxvel expt)))))


;%%%%%%%%% SCORE TOOLS %%%%%%%%%%%%%

(defmethod! atoms->chords ((self sdiffile) &optional mintime maxtime)
            :initvals '(nil nil nil)
            :icon 03
            (let ((sdiflist (flat (getsdifdata self 0 "XADT" "XADT" nil nil nil mintime maxtime) 1)))
              (setf chordlist
                    (loop for atom in sdiflist collect
                          (make-instance 'chord 
                                         :lmidic (list (f->mc (third atom)))
                                         :lvel (list (om-scale (fourth atom) 0 127 0 (list-max (fourth (mat-trans sdiflist)))))
                                         :loffset (list ;this needs samples->ms 
                                                   (first atom))
                                         :ldur (list (second atom))
                                         )))
              chordlist))

(defmethod! chords->chordseq ((self list) &optional legato)
;later add quantization functions when converting into a real score...
            :icon 01
            :initvals '(nil 0)
            (let ((tchordseq (objfromobjs self 'chord-seq)))
              (print tchordseq)
                   ;(make-instance 'chord-seq :self self)))
            (setf onsets
                  (flat 
                   (loop for chord in self collect
                         (loffset chord))))
            (setf final-chordseq
                  (make-instance 'chord-seq 
                                 :lmidic (lmidic tchordseq)
                                 :lonset onsets 
                                 :ldur (ldur tchordseq)
                                 :lvel (lvel tchordseq)
                                 :lchan (lchan tchordseq)
                                ; :legato legato
                                 ))
            (print (lmidic tchordseq))
            final-chordseq
            ))
