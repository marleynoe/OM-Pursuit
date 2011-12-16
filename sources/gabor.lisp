;AtomicOrchestrator, 2010 McGill University
;
;This program is free software; you can redistribute it and/or
;modify it under the terms of the GNU General Public License
;as published by the Free Software Foundation; either version 2
;of the License, or (at your option) any later version.
;
;See file LICENSE for further informations on licensing terms.
;
;This program is distributed in the hope that it will be useful,
;but WITHOUT ANY WARRANTY; without even the implied warranty of
;MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;GNU General Public License for more details.
;
;You should have received a copy of the GNU General Public License
;along with this program; if not, write to the Free Software
;Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307,10 USA.
;
;Authors: M. Schumacher

(in-package :om)

; I need a function to convert an array back into an SDIF file.

(defmethod! gabor-params (&key (markers '((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2))) (duration '(1/2 1/4 1/8 1/16)) (freq-range (1 2 3 4)) 
                             (freq-resolution (1 2 3 4)) (max-atoms 10) (min-deviation 0.001))
                        :icon 04
                        :initvals '(((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2)) (1/2 1/4 1/8 1/16) (1 2 3 4) (1 2 3 4) 10 0.001)
                        :indoc '("markers (sec)" "duration relative to partial" "freq-range around the partial (Hz)" "freq-resolution around the partial (Hz)" "max number of atoms" "min-deviation")
                        ;:doc "Apply a tremolo (low frequency amplitude modulation) effect to the audio. The tremolo frequency in Hz is given by speed (default 5), and the depth as a percentage by depth (default 40)"
                        (let* (
                               (thestring (format nil "'~a' '~a' '~a' '~a' ~d ~d" 
                                                  (python-format (om/ 1 duration)) 
                                                  markers
                                                  (python-format (om-float freq-range)) 
                                                  (python-format (om-float freq-resolution)) 
                                                  max-atoms min-deviation)))
                          thestring))



;maybe make a 'decomp-2-score function... that calls atoms->chords and also gets samplerate, filename, etc.

(defmethod! gabor-decomp ((snd sound) (partials sdiffile) (commands string) &key outpath (molecules 0))
            :icon 04
            :numouts 3
            :initvals '(nil nil nil nil 0)
            :outdoc '("decomposition-sdif-file" "decomposition-audio-file" "residual-audio-file")
            ;:menuins
            (if (probe-file *gabor-path*)
                (let* ((inpath (sound-path snd))
                       (partialsfile (namestring (filepathname partials)))
                       (filename (pathname-name inpath))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a_gabor" filename)))
                       (outdir (or outpath (make-pathname :directory (append (pathname-directory inpath) (list name)))))                      
                       (sdif-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                       :name (string+ filename "_tonal_decomp") :type "sdif"))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_gabor-synthesis") :type "wav"))
                       (residual-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_gabor-residual") :type "wav"))
                       )
                  (om-create-directory outdir)
                  (setf str 
                        (format nil "~s ~s ~s ~s ~a ~d" 
                                (namestring *gabor-path*)
                                (namestring inpath)
                                (namestring partialsfile)
                                (namestring outdir)
                                commands
                                molecules
                                ))
                  ;(print str)
                  ;(print outdir)
                  ;(print sdif-outfile)
                  ;(print audio-outfile)
                  ;(print residual-outfile)
                  (om-cmd-line str *sys-console*)
                  (values (probe-file sdif-outfile) (probe-file audio-outfile) (probe-file residual-outfile))           
                  )
              (progn 
                (print "gaborDecomp not found... set in preferences?")
                nil
                ))
            )

(defmethod! get-gabor-params ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 9
            :outdoc '("numatoms" "onset" "duration" "magnitude" "norm" "frequency" "phase" "bandwidth" "molecule")
            (let* ((sdiflist (flat (getsdifdata self 0 "XADT" "XGAM" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist))
                   (samplerate (get-decomp-fs self))
                   (reci-fs (/ 1 samplerate)))
              (values 
               (length sdiflist)                    ;numatoms              
               (om* reci-fs (second translist))     ;onset (sec)
               (om* reci-fs (third translist))      ;duration (sec)
               (fifth translist)                    ;magnitude (lin)
               (seventh translist)                  ;norm (lin)                  
               (om* samplerate (fourth translist))  ;frequency (Hz)              
               (fifth translist)                    ;phase (rad)
               (gabor-bandwidth (third translist))  ;bandwidth (Hz)) 
               (om-round (first translist)))        ;molecule (int))  
              ))

(defmethod! get-gabor-array ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the gabor array")
            (let* ((sdifdata (multiple-value-list (get-gabor-params self mintime maxtime)))
                   (thearray (make-instance 'gabor-array 
                                            :numcols (first sdifdata)
                                            :onset (second sdifdata)
                                            :duration (third sdifdata)
                                            :magnitude (fourth sdifdata)
                                            :norm (fifth sdifdata)
                                            :frequency (sixth sdifdata)
                                            :phi (seventh sdifdata)
                                            :bandwidth (nth 7 sdifdata)
                                            :molecule (nth 8 sdifdata))))
              thearray))

(defmethod! get-gabor-array-resamp ((self sdiffile) (numcols integer) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled gabor array")
            (let* ((gabordata (multiple-value-list (get-gabor-params self mintime maxtime)))
                   (thearray (make-instance 'gabor-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second gabordata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third gabordata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth gabordata) 'bpf 15)
                                            :norm (simple-bpf-from-list '(0 1) (fifth gabordata) 'bpf 15)
                                            :frequency (simple-bpf-from-list '(0 1) (sixth gabordata) 'bpf 15)
                                            :phi (simple-bpf-from-list '(0 1) (nth 6 gabordata) 'bpf 15)
                                            :bandwidth (simple-bpf-from-list '(0 1) (nth 7 gabordata) 'bpf 15)
                                            :molecule (simple-bpf-from-list '(0 1) (nth 8 gabordata) 'bpf 0))))
              thearray))


(defmethod objfromobjs ((self sdiffile) (type gabor-array))
  (let* ((sdifdata (multiple-value-list (get-gabor-params self)))
         (new (make-instance 'gabor-array 
               :numcols (first sdifdata)
               :onset (second sdifdata)
               :duration (third sdifdata)
               :magnitude (fourth sdifdata)
               :norm (fifth sdifdata)
               :frequency (sixth sdifdata)
               :phi (nth 6 sdifdata)
               :bandwidth (nth 7 sdifdata)
               :molecule (nth 8 sdifdata)))
         )
  new))

;make a menu: 'no molecules' / 'molecules' / 'trimmed molecules' 
;also...

(defmethod! adt-synthesis ((self sdiffile) (molecules number) (trim number) &key outpath)
            :icon 607
            :indoc '("an XADT-sdif-file" "molecules flag" "trim flag" "optional outpath")
            :numouts 3
            :initvals '(nil 0 1 nil)
            :outdoc '("decomposition-audio-file" "list of molecules" "list of molecule onsets")
            ;:menuins
            (if (probe-file *adt-synth-path*)
                (let* (
                       (sdif-path (filepathname self))
                       (filename (pathname-name sdif-path))
                       ;(outpath (progn
                                 ; (when *automatic-rename*
                                 ; (handle-new-dir-exists outpath))))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a_adt" filename)))
                       (outdir (progn
                                  (if *automatic-rename*
                                    (progn
                                      (or (handle-new-dir-exists outpath) 
                                          (handle-new-dir-exists (make-pathname :directory (append (pathname-directory sdif-path) (list name))))))
                                     (or outpath (make-pathname :directory (append (pathname-directory sdif-path) (list name)))))
                                  ))
                       (moldir (om-make-pathname :directory ;(format nil "~a~a_molecules" (namestring outdir) filename))) 
                                              (append (pathname-directory outdir) (list (format nil "~a_molecules" filename))))) 
                                              ; (list outdir) (list (format nil "~a_molecules" filename)))))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_synthesis") :type "wav"))
                       (onsets-outfile (om-make-pathname :directory (append (pathname-directory moldir))
                                                        :name (string+ filename "_onsets") :type "txt"))
                       )

                  (om-create-directory outdir)
                  (om-create-directory moldir)
                  (setf str 
                        (format nil "~s ~s ~s ~D ~D" 
                                (namestring *adt-synth-path*)
                                (namestring sdif-path)
                                (namestring outdir)
                                molecules
                                trim                              
                                ))
                  
                  ;(print str)
                  ;(print outdir)
                  ;(print moldir)
                  ;(print sdif-outfile)
                  ;(print audio-outfile)
                  ;(print residual-outfile)

                  ;later I should have auto-renaming, removal of temporary files, etx. HERE

                  (om-cmd-line str *sys-console*)
                  (if (equal molecules 1)
                      (values (probe-file audio-outfile) (om-directory moldir :type '("wav" "aif")) (second (first (read-file onsets-outfile))))
                    (probe-file audio-outfile))
                  )
              (progn 
                (print "atomSynth not found... set in preferences?")
                nil
                ))
            )

; HELPER FUNCTIONS

(defun read-file (self)
 (with-open-file (f self :direction :input)
   (let ((line (read-line f nil 'eof))
         (rep nil))
     (loop while (not (equal line 'eof)) do
           (multiple-value-bind (name rest)
               (string-until-char 
                (remove-if #'(lambda (c) (or (= 194 c) (= 160 c))) line :key 'char-code)
                ":")samplerate
             (when name
               (if rest
                   (pushr (list name (read-from-string rest)) rep)
                 (let ((linedata (data-from-line name)))
                   (pushr (list (apply 'concatenate (cons 'string
                                                          (mapcar #'(lambda (item)
                                                                      (concatenate 'string (string item) " "))
                                                                  (butlast linedata))))
                                (car (last linedata)))
                          rep))))
             (setf line (read-line f nil 'eof)))
           )
     rep)))




(defun gabor-bandwidth (duration)
  (om/ 8.2 duration))


#|
(defun gabor-amplitude (norm magnitude)
  (om/ 1 (om-abs (om* norm magnitude))))
|#

(defun gabor-amplitude (norm magnitude)
  (om* (om/ 1  norm) (om-abs magnitude)))

;; maybe samplerate is not needed, since 'duration' is in sec, not in samples like 'scale'