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

(defmethod! fof-params (&key (markers '((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2))) (duration '(1/2 1/4 1/8 1/16)) (rise-time '(1/2 1/4 1/8 1/16)) (freq-range (1 2 3 4)) 
                             (freq-resolution (1 2 3 4)) (max-atoms 10) (min-deviation 0.001))
                        :icon 04
                        :initvals '(((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2)) (1/2 1/4 1/8 1/16) (1/2 1/4 1/8 1/16) (1 2 3 4) (1 2 3 4) 10 0.001)
                        :indoc '("markers (sec)" "duration ratio of partial" "rise-time ratio of duration" "freq-range around the partial (Hz)" "freq-resolution around the partial (Hz)"  "max number of atoms" "min-deviation")
                        ;:doc "Apply a tremolo (low frequency amplitude modulation) effect to the audio. The tremolo frequency in Hz is given by speed (default 5), and the depth as a percentage by depth (default 40)"
                        (let* (
                               (thestring (format nil "'~a' '~a' '~a' '~a' '~a' ~d ~d" 
                                                  (python-format (om/ 1 duration)) 
                                                  markers
                                                  (python-format (om-float freq-range)) 
                                                  (python-format (om-float freq-resolution))
                                                  (python-format (om/ 1 rise-time)) 
                                                  max-atoms min-deviation)))
                          thestring))




;maybe make a 'decomp-2-score function... that calls atoms->chords and also gets samplerate, filename, etc.

; IT WOULD BE BETTER, IF THERE WOULD BE ONLY ONE EXECUTABLE AND A FLAG FOR SETTING FOF VS GABOR VS SOUNDGRAINS. THIS FLAG COULD IN OM BE SET BY THE INDIVIDUAL 'PARAM' FUNCTIONS, SO THAT THERE WILL BE ONLY ONE 'DECOMP' RENDERING ENGINE, LIKE IN OMSOX...

(defmethod! fof-decomp ((snd sound) (partials sdiffile) (commands string) &key outpath (molecules 0))
            :icon 04
            :numouts 3
            :initvals '(nil nil nil nil 0)
            :outdoc '("decomposition-sdif-file" "decomposition-audio-file" "residual-audio-file")
            ;:menuins
            (if (probe-file *fof-path*)
                (let* ((inpath (sound-path snd))
                       (partialsfile (namestring (filepathname partials)))
                       (filename (pathname-name inpath))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a_fof" filename)))
                       (outdir (or outpath (make-pathname :directory (append (pathname-directory inpath) (list name)))))                      
                       (sdif-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                       :name (string+ filename "_tonal_decomp") :type "sdif"))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_tonal_decomp") :type "wav"))
                       (residual-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_tonal_decomp_residual") :type "wav"))
                       )
                  (om-create-directory outdir)
                  (setf str 
                        (format nil "~s ~s ~s ~s ~a ~d" 
                                (namestring *fof-path*)
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
                (print "fofDecomp not found... set in preferences?")
                nil
                ))
            )

(defmethod! get-fof-params ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 12
            :outdoc '("numatoms" "onset" "duration" "magnitude" "norm" "frequency"  "phase" "bandwidth" "risetime" "decaytime" "skirt-width" "molecule")
            (let* ((sdiflist (flat (getsdifdata self 0 "XADT" "XFOM" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist))
                   (samplerate (get-decomp-fs self))
                   (reci-fs (/ 1 samplerate))
                   (risetime (fifth translist))
                   (decaytime (sixth translist)))
              (values 
               (length sdiflist)                    ;numatoms             
               (om* reci-fs (second translist))     ;onset (sec)
               (om* reci-fs (third translist))      ;duration (sec)          
               (seventh translist)                  ;magnitude (lin)
               (nth 8 translist)                    ;norm (lin)
               (om* samplerate (fourth translist))  ;frequency (Hz)
               (nth 7 translist)                    ;phase (rad)
               (get-fof-bandwidth decaytime samplerate) ;bandwidth (Hz)
               (om/  risetime samplerate)           ;risetime (sec)
               (om/ decaytime samplerate)           ;decaytime (sec)
               (get-skirt-width risetime samplerate);skirt-width (Hz)
               (om-round (first translist))         ;molecule (int)
               )
              ))


(defun fof-amplitude (norm magnitude)
  (om/ 1 (om-abs (om* norm magnitude))))


(defmethod! get-fof-array ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the fof array")
            (let* ((thedata (multiple-value-list (get-fof-params self mintime maxtime)))
                   (thearray (make-instance 'fof-array 
                                            :numcols (first thedata)
                                            :onset (second thedata)
                                            :duration (third thedata)
                                            :magnitude (fourth thedata)
                                            :norm (fifth thedata)
                                            :frequency (sixth thedata)
                                            :phi        (nth 6 thedata)
                                            :bandwidth  (nth 7 thedata)
                                            :risetime   (nth 8 thedata)
                                            :decaytime (nth 9 thedata)
                                            :skirt-width (nth 10 thedata)
                                            :molecule (nth 11 thedata))))
              thearray))

(defmethod! get-fof-array-resamp ((self sdiffile) (numcols integer) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled fof array")
            (let* ((thedata (multiple-value-list (get-fof-params self mintime maxtime)))
                   (thearray (make-instance 'fof-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second thedata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third thedata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth thedata) 'bpf 15)
                                            :norm (simple-bpf-from-list '(0 1) (fifth thedata) 'bpf 15)
                                            :frequency (simple-bpf-from-list '(0 1) (sixth thedata) 'bpf 15)
                                            :phi (simple-bpf-from-list '(0 1) (nth 6 thedata) 'bpf 15)
                                            :bandwidth (simple-bpf-from-list '(0 1) (nth 7 thedata) 'bpf 15)
                                            :risetime (simple-bpf-from-list '(0 1) (nth 8 thedata) 'bpf 15)
                                            :decaytime (simple-bpf-from-list '(0 1) (nth 9 thedata) 'bpf 15)
                                            :skirt-width (simple-bpf-from-list '(0 1) (nth 10 thedata) 'bpf 15)
                                            :molecule (simple-bpf-from-list '(0 1) (nth 11 thedata) 'bpf 0))))
              thearray))

(defmethod objfromobjs ((self sdiffile) (type fof-array))
  (let* ((sdifdata (multiple-value-list (get-fof-params self)))
         (new (make-instance 'fof-array 
               :numcols (first sdifdata)
               :onset (second sdifdata)
               :duration (third sdifdata)
               :magnitude (fourth sdifdata)
               :norm (fifth sdifdata)
               :frequency (sixth sdifdata)
               :phi (nth 6 sdifdata)
               :bandwidth (nth 7 sdifdata)
               :risetime (nth 8 sdifdata)
               :decaytime (nth 9 sdifdata)
               :skirt-width (nth 10 sdifdata)
               :molecule (nth 11 sdifdata)))
         )
  new))

;--------- HELPER FUNCTIONS -------------

(defun get-skirt-width (risetime samplerate)
 (om* (om/ pi risetime) samplerate))

(defun get-fof-bandwidth (decaytime samplerate)
 (om*  (om/ (om-log decaytime) decaytime ) samplerate))



;maybe I don't need to multiply with samplerate since I have rise and decaytime in secs already
