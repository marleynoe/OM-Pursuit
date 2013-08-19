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

;%%%%%%%%%%%%%% PLOTTING FUNCTIONS %%%%%%%%%%%%%%%

(defun add-pict-to-maquette (pictpath maquette t1 t2 y1 y2)
 (let* ((picture-handler (om-load-pixmap (pathname-name pictpath) (pathname-type pictpath) 
                                         (make-pathname :directory (pathname-directory pictpath) 
                                                        :host (pathname-host pictpath) :device (pathname-device pictpath))))
        (pict (make-instance 'picture :name "my-picture"
                             :thepict picture-handler
                             :pict-pathname pictpath
                             :draw-params (list 'c t1 y1 t2 y2))))
   (setf (pictu maquette) pict)
   t))

(defmethod! clearmaq ((self OMMaquette))
   :icon '(327)
   :initvals '(nil)
   :indoc '("a maquette")
   :doc "Removes all TemporalBoxes in <self>."
   (removeTemporalBox (TemporalBoxes self)))

(defun between? (point lowbound highbound)
  (and (< lowbound point) (> highbound point))
      )

(defun inbetween? (point pointlist)
  (and (< (list-min pointlist) point) (> (list-max pointlist) point))
      )

(defmethod! partials->bpflib ((self sdiffile) &key mintime maxtime) ;(frametype "1TRC"))
            :icon '(141)
            :numouts 1
            (multiple-value-bind (framelist timelist) (getsdifdata self 0 "1TRC" "1TRC" nil nil nil mintime maxtime)
            (setf partial-list (loop for frame in framelist 
                                     for time in timelist collect    
                                     (loop for partial in frame collect
                                           (x-append partial time))
                                     )))
            (setf grouped-partials (group-indices (flat partial-list 1)))
            (loop for partial in grouped-partials collect
                  (let ((thepartial (mat-trans (cdr partial))))
                  (simple-bpf-from-list (fourth thepartial) (first thepartial) 'bpf 10)
                  )))

#|
(defmethod! partials->3DClib ((self sdiffile) &key mintime maxtime (mode logarithmic))
            :icon '(141)
            :numouts 1
            :menuins
            (multiple-value-bind (framelist timelist) (getsdifdata self 0 "1TRC" "1TRC" nil nil nil mintime maxtime)
            (setf partial-list (loop for frame in framelist 
                                     for time in timelist 
                                     collect    
                                     (loop for partial in frame collect
                                           (x-append partial time))
                                     )))
            (setf grouped-partials (group-indices (flat partial-list 1)))
            ;(print mode)
            (when (equal mode 'linear)
              (loop for partial in grouped-partials collect
                    (let ((thepartial (mat-trans (cdr partial))))
                      (3dc-from-list (om* 1000 (fourth thepartial)) (first thepartial) (om* 1000 (second thepartial)) '3dc 10) ;x=time, y=freq, z=amp
                    )
              ))
            (when (equal mode 'logarithmic)
              (loop for partial in grouped-partials collect
                    (let ((thepartial (mat-trans (cdr partial))))
                      (3dc-from-list (om* 1000 (fourth thepartial)) (first thepartial) (om* 10 (lin->db (second thepartial))) '3dc 10) ;x=time, y=freq, z=amp
                      )))
            )
|#

(defun y-offset (thebpf offset)
  (let* ((xpoints (x-points thebpf))
        (ypoints (y-points thebpf))
        (newy (om+ ypoints offset)))
    (list (x-append (first xpoints) xpoints (last xpoints)) (x-append (first ypoints) newy (last ypoints))))
        )

; could later add resolution

(defun draw-bpf-rectangle (position horizontalsize verticalsize)
  (let* ((xpos (first position))
         (ypos (second position))
         (x-extend (+ xpos horizontalsize))
         (y-extend (- ypos verticalsize)))
    (simple-bpf-from-list (list xpos x-extend x-extend xpos xpos) (list ypos ypos y-extend y-extend ypos) 'bpc 10)
    ))

(defun draw-centered-rectangle (position horizontalsize verticalsize)
  (let* ((xpos (first position))
         (ypos (second position))
         (x-extend (+ xpos horizontalsize))
         (y-positive (+ ypos (* .5 verticalsize)))
         (y-negative (- ypos (* .5 verticalsize))))    
    (simple-bpf-from-list (list xpos x-extend x-extend xpos xpos) (list y-positive y-positive y-negative y-negative y-positive) 'bpc 10)
    ))

(defun make-centered-temporalbox (position horizontalsize verticalsize)
  ;possibly rescale the time-data
  (let* ((xpos (first position))
         (ypos (second position))
         )
  (make-instance 'temporalbox
                 :offset xpos
                 :extend horizontalsize
                 :posy (+ ypos (* .5 verticalsize))
                 :sizey verticalsize
                 :value '""
                 )
  ))


(defun get-bpf-dur (bpf)
  (- (list-max (x-points bpf)) (list-min (x-points bpf)))
     )


(defmethod! convert-paths ((self t) (discard-levels number) (new-dir t) (keep-levels number))
            (let* ((orig-dir (pathname-directory self))
                   (orig-path (x-diff orig-dir (first-n orig-dir discard-levels)))
                   (thenew-dir (pathname-directory new-dir))
                   (new-path (first-n thenew-dir keep-levels))
                   (thefinal-dir (x-append new-path orig-path))
                   (thefilename (pathname-name self))
                   (thetype (pathname-type self)))
              (make-pathname :directory thefinal-dir :name thefilename :type thetype)))

(defmethod! convert-paths ((self list) (discard-levels number) (new-dir t) (keep-levels number))
            (mapcar (lambda (thepaths)
                      (convert-paths thepaths discard-levels new-dir keep-levels)) self))
                   

; %%%%%%%%%%%% FOR DRAWING BPCS (RECTANGLES) REPRESENTING GABOR SPACES %%%%%%%%%% 

(defmethod! gabor-spaces ((markers list) (bpfs list) (scales number) (ranges number))
            :icon 04
            :numouts 3
            (let* ((xmin (list-min markers))
                   (thelist       
                   (loop for marker in markers collect
                         (loop for bpf in bpfs collect
                               (if (inbetween? marker (x-points bpf))
                                   (let* ((bpfdur (get-bpf-dur bpf))
                                          (xextend (* bpfdur scales))
                                          (xmax (+ marker xextend))
                                          (ymin (- (list-min (y-points bpf)) (* 0.5 ranges)))
                                          (ymax (+ (list-max (y-points bpf)) (* 0.5 ranges))))
                                     (unless (> xmax (list-max (x-points bpf)))
                                       (list (draw-centered-rectangle 
                                              (list marker (x-transfer bpf (+ marker (* 0.5 xextend))))
                                              xextend
                                              ranges)
                                             xmax
                                             ymin
                                             ymax)))
                                     ))))
                   (thetranslist (mat-trans (remove nil (flat thelist 1)))))
               (values (bpf-colour (first thetranslist) :r (om-random 0.5 1.0) :g (om-random 0.5 1.0) :b (om-random 0.5 1.0))
                      (list
                        xmin (list-max (second thetranslist)))
                      (list
                       (list-min (third thetranslist)) (list-max (fourth thetranslist)))
                      )
               ))

(defmethod! gabor-spaces ((markers list) (bpfs list) (scales list) (ranges list))
            (let ((theresult
                   (loop 
                   ; for marker in markers
                    for scale in scales
                    for range in ranges
                         collect
                         (multiple-value-list (gabor-spaces markers bpfs scale range))))
                         )
              (let* ((thetranslist (mat-trans theresult))
                     (thex (mat-trans (second thetranslist)))
                     (they (mat-trans (third thetranslist))))               
              (values (flat (first thetranslist))
                      (list (list-min (first thex)) (list-max (second thex)))
                      (list (list-min (first they)) (list-max (second they))))
              )))

; %%%%%%%%%%%% for drawing tfregions %%%%%%%%%%%%% COULDN'T I USE THIS SIMPLY FOR THE DRAWING INTO THE MAQUETTE?

(defmethod! draw-tfregions ((markers list) (bpfs list) (scales number) (ranges number))
            :icon 04
            :numouts 5
            (let* ((xmin (list-min markers))
                   (thelist       
                   (loop for marker in markers collect
                         (loop for bpf in bpfs collect
                               (if (inbetween? marker (x-points bpf))
                                   (let* ((bpfdur (get-bpf-dur bpf))
                                          (xextend (* bpfdur scales))
                                          (xmin marker)
                                          (xmax (+ marker xextend))
                                          (ymin (- (list-min (y-points bpf)) (* 0.5 ranges)))
                                          (ymax (+ (list-max (y-points bpf)) (* 0.5 ranges))))
                                     ;(unless (> xmax (list-max (x-points bpf)))
                                       (list (make-centered-temporalbox
                                              (list marker (x-transfer bpf (+ marker (* 0.5 xextend))))
                                              xextend
                                              ranges)
                                             xmin
                                             xmax
                                             ymin
                                             ymax);)
                                     ;(print bpfdur)
                                     )
                                     ))))
                   (thetranslist (mat-trans (remove nil (flat thelist 1)))))
              #|
              (values  (bpf-colour (first thetranslist) :r (om-random 0.5 1.0) :g (om-random 0.5 1.0) :b (om-random 0.5 1.0))
                        (list
                       xmin (list-max (second thetranslist)))
                      (list
                       (list-min (third thetranslist)) (list-max (fourth thetranslist)))
                       |#
              
              (values thetranslist
                      (second thetranslist)
                      (third thetranslist)
                      (fourth thetranslist)
                      (fifth thetranslist)
                      )
               ))
#|
(defmethod! make-tfregions ((markers list) (bpfs list) (scales list) (ranges list))
            (let ((theresult
                   (loop 
                   ; for marker in markers
                    for scale in scales
                    for range in ranges
                         collect
                         (multiple-value-list (make-tf-regions markers bpfs scale range))))
                         )
              (let* ((thetranslist (mat-trans theresult))
                     (thex (mat-trans (second thetranslist)))
                     (they (mat-trans (third thetranslist))))               
              (values (flat (first thetranslist))
                      (list (list-min (first thex)) (list-max (second thex)))
                      (list (list-min (first they)) (list-max (second they))))
              )))
|#


(defmethod! find-bpflib-dimensions (bpflibs)
            :numouts 4
  (let* ((thevallist (mat-trans
         (loop for bpf in bpflibs collect
               (list (list-min (x-points bpf))
                     (list-max (x-points bpf))
                     (list-min (y-points bpf))
                     (list-max (y-points bpf))))))
        (xmin (om-round (* 1000 (list-min (first thevallist)))))
        (xmax (om-round (* 1000 (list-max (second thevallist)))))
        (ymin (om-round (list-min (third thevallist))))
        (ymax (om-round (list-max (fourth thevallist)))))
        (values xmin xmax ymin ymax)
        ))
              

(defmethod! partials->3DClib ((self sdiffile) &key mintime maxtime (mode "linear"))
            :icon '(141)
            :numouts 1
            :initvals '(nil nil nil "linear")
            :menuins '((3 (("linear" "linear") ("logarithmic" "logarithmic"))))
 
            (multiple-value-bind (framelist timelist) (getsdifdata self 0 "1TRC" "1TRC" nil nil nil mintime maxtime)
            (setf partial-list (loop for frame in framelist 
                                     for time in timelist 
                                     collect    
                                     (loop for partial in frame collect
                                           (x-append partial time))
                                     )))
            (setf grouped-partials (group-indices (flat partial-list 1)))
            (print mode)
            (when (equal mode "linear")
              (setf output (loop for partial in grouped-partials collect
                                 (let ((thepartial (mat-trans (cdr partial))))
                                   (3dc-from-list (om* 1000 (fourth thepartial)) (first thepartial) (om* 10000 (second thepartial)) '3dc 10) ;x=time, y=freq, z=amp
                                   ;(3dc-from-list (fourth thepartial) (first thepartial) (om* 1000 (second thepartial)) '3dc 10)
                                   )))
              )
            (when (equal mode "logarithmic")
              (setf output (loop for partial in grouped-partials collect
                                 (let ((thepartial (mat-trans (cdr partial))))
                                   (3dc-from-list (om* 1000 (fourth thepartial)) (first thepartial) (om* 10 (lin->db (second thepartial))) '3dc 10) ;x=time, y=freq, z=amp
                                   )))
                    )
            output)


;(defmethod! color ((midicent list))"numatoms" "onset" "duration" "magnitude" "norm" "frequency" "phase" "bandwidth" "molecule"
;(setf (bpfcolor XXXX) (om-make-color r g b))


;(lambda (bpf color) (setf (bpfcolor bpf) color))

;(lambda (bpflib r g b) 
;      (let ((newbpflib (clone bpflib)))
;           (loop for bpf in (bpf-list newbpflib) do
;                  (setf (bpfcolor bpf) (om-make-color r g b)))
;           newbpflib))

;(lambda (bpf r g b) 
;      (let ((newbpf (clone bpf)))
;           (setf (bpfcolor newbpf) (om-make-color r g b))
;           newbpf))


#|
(defmethod! partials->3DClib ((self sdiffile) &key mintime maxtime)
            :icon '(141)
            :numouts 1
            (multiple-value-bind (framelist timelist) (getsdifdata self 0 "1TRC" "1TRC" nil nil nil mintime maxtime)
            (setf partial-list (loop for frame in framelist 
                                     for time in timelist 
                                     collect    
                                     (loop for partial in frame collect
                                           (x-append partial time))
                                     )))
            (setf grouped-partials (group-indices (flat partial-list 1)))
            (loop for partial in grouped-partials collect
                  (let ((thepartial (mat-trans (cdr partial))))
                  (3dc-from-list (om* 1000 (fourth thepartial)) (first thepartial) (om* -1000 (second thepartial)) '3dc 10) ;x=time, y=freq, z=amp
                  )))
|#
#|
(defclass partials-viewer (editorview) 
  ((streampanels :initform nil :accessor streampanels))
  (:default-initargs))

(defmethod class-has-editor-p ((self gesture-array)) t)
(defmethod get-editor-class ((self gesture-array)) 'gesture-editor)
|#

;should give the bpfs different colors depending on the amplitude

;%%%%%%%%%%%%%% HELPER FUNCTIONS %%%%%%%%%%%%%%%

(defmethod! get-bpf-points ((self bpf-lib))
            (let ((bpflist (bpf-list self)))
                    (loop for bpf in bpflist collect
                    (point-pairs bpf)
                    ))
              )
; sort this?

(defmethod! group-indices ((self list))
 (let ((reslist nil))
   (loop for item in self do
         (let ((position (position (car item) reslist :test '= :key 'car)))
           (if position
               (pushr (cdr item) (nth position reslist))
             (pushr (list (car item) (cdr item)) reslist))))
   (mapcar 'identity reslist)))

(defun get-decomp-fs (sdiffile)
  (read-from-string (find-in-nvtlist (getnvtlist sdiffile) "sample rate")))

;%%%%%%%%%%%%%% FORMATTING FUNCTIONS %%%%%%%%%%%%%%%

(defmethod! markers->frames ((self list) &key samplerate)
            :icon '(141)
            (when (listp (first self))
              (setf out-array (adt-deep-format (om-round (sec->samples self (or samplerate *audio-sr*))))))
            (when (numberp (first self))
              (setf out-array (adt-format (om-round (sec->samples self (or samplerate *audio-sr*))))))
            (when (sound-p (first self))
              (setf out-array (adt-deep-format (om-round
                               (loop for snd in self collect
                                     (sec->samples (markers snd) (om-sound-sample-rate snd)))))))
              out-array
              )

(defmethod! markers->frames ((self sound) &key samplerate)
                   (let ((out-array (python-format (om-round (sec->samples (markers self) (om-sound-sample-rate self))))))
            out-array
            ))

(defmethod sound-p ((self sound)) t)
(defmethod sound-p ((self t)) nil) 


; get any kind of SDIF marker or onsets in voices or chord-seqs -> compare with OM-SoX
; i should distinguish between specdistance-positive and specdistance-negative  if there's a distinction in the SDIF
; Also, it should already look for the specifc markers once they are out.. it shouldn't need a 't

(defmethod! get-sdif-markers ((self sdiffile) &key specdistance transient manual quantize mintime maxtime)
            :initvals '(t t t t nil nil nil) ; all markers unquantized over the entire range
            :icon '(608)
            (let ((markers nil)
                  (frametimes (getsdiftimes self 0 "1MRK" nil mintime maxtime)))
                   (when specdistance (setf markers (x-append markers (getsdiftimes self 0 "1MRK" "XASD" mintime maxtime))))
                   (when transient (setf markers (x-append markers (getsdiftimes self 0 "1MRK" "1BEG" mintime maxtime))))
                   (when manual (setf markers (x-append markers 
                                                      (x-diff frametimes (x-append 
                                                                          (getsdiftimes self 0 "1MRK" "XASD" mintime maxtime) 
                                                                          (getsdiftimes self 0 "1MRK" "1BEG" mintime maxtime) 
                                                                          (getsdiftimes self 0 "1MRK" "1END" mintime maxtime))))))
                   (when quantize
                       (setf markers (quantize markers quantize)))
                   (sort-list markers)
              ))

(defmethod! get-sdif-markers ((self chord-seq) &key specdistance transient manual quantize mintime maxtime)
            (let* ((markers (om* 0.001 (lonset self))))
                   (when quantize
                       (setf markers (quantize markers quantize)))
                   (sort-list markers)
              ))

(defmethod! get-sdif-markers ((self voice) &key specdistance transient manual quantize mintime maxtime)
            (get-sdif-markers (ObjfromObjs self (mki 'chord-seq)) :quantize quantize :mintime mintime :maxtime maxtime)
              )

(defmethod! get-sdif-markers ((self multi-seq) &key specdistance transient manual quantize mintime maxtime)
            (mapcar (lambda (thechordseqs)
                      (get-sdif-markers thechordseqs)) (chord-seqs self)))

(defmethod! get-sdif-markers ((self poly) &key specdistance transient manual quantize mintime maxtime)
            (mapcar (lambda (thevoices)
                      (get-sdif-markers thevoices)) (voices self)))

(defmethod! get-sdif-markers ((self list) &key specdistance transient manual quantize mintime maxtime)
            (mapcar (lambda (thelist)
                      (get-sdif-markers thelist 
                                   :specdistance specdistance :transient transient :manual manual 
                                   :quantize quantize :mintime mintime :maxtime maxtime)) self))


;;; --- format for the python-executable -----

(defun python-deep-format (input-list)
  (setf thestring (format nil "[~a" (python-format (car input-list))))
  (loop for item in (cdr input-list) collect
         (setf thestring (concatenate 'string thestring "," (python-format item))))
  (setf thestring (concatenate 'string thestring (format nil "]")))
  thestring)


(defun python-format (input-list)
  (setf thestring (format nil "[~d" (first input-list)))
        (loop for item in (cdr input-list) do
              (setf thestring (concatenate 'string thestring
                                           (format nil ",~d" item))))
        (setf thestring (concatenate 'string thestring (format nil "]")))
        thestring)

(defun python-format-nc (input-list)
  (setf thestring (format nil " ~d" (first input-list)))
        (loop for item in (cdr input-list) do
              (setf thestring (concatenate 'string thestring
                                           (format nil " ~d" item))))       
        thestring)

(defun python-format-s (input-list)
  (setf thestring (format nil "['~d'" (first input-list)))
        (loop for item in (cdr input-list) do
              (setf thestring (concatenate 'string thestring
                                           (format nil ",'~d'" item))))
        (setf thestring (concatenate 'string thestring (format nil "]")))
        thestring)

(defun python-directories (dirs)
  (let* ((thepaths (list! dirs))
        (directories (mapcar (lambda (thedirs)
                               (namestring thedirs)) thepaths)))
  (python-format-s directories)))


;compat

(defun adt-format (input-list)
  (python-format input-list))

(defun adt-deep-format (input-list)
  (python-deep-format input-list))


(defun om-float (input)
  (loop for item in input collect
        (float item)
        ))

;-------------------


(defmethod! get-soundgrain-paths ((self sdiffile) (corpus list) (file list))
            (mapcar (lambda (corpora files)
                      (get-soundgrain-paths self corpora files)) corpus file)
            )

;-------------------
(defmethod! get-soundgrain-paths ((self sdiffile) (corpus number) (file number))
  (let* ((nvtlist (getnvtlist self))
         (corpus (round corpus))
         (file (round file))
         (corpusname (find-in-nvt (second nvtlist) (integer-to-string corpus)))
         (filename (find-in-nvt (nth (+ 2 corpus) nvtlist) (integer-to-string file)))
         (thepath (pathname (format nil "~a/~a" corpusname filename))))
        thepath
  ))
  
; =============================



(defmethod! my-string-until-last-char ((string string) (char string))
            :numouts 2
            (let ((index (search char string :from-end t)))
              (if index 
                  (values (subseq string 0 index) (subseq string (+ index 1)))
                (values string nil))))

(defmethod! my-string-until-char ((string string) (char string))
            :numouts 2
            (let ((index (search char string)))
              (if index (values (subseq string 0 index) (subseq string (+ index 1)))
                (values string nil))))

(defmethod! transratio ((midicent number))
            :icon '(141)  
            :initvals '(0)
            :indoc '("converts transposition in midicents into transposition-factor")
            :numouts 1
            :doc "bla bla"
 (exp (* midicent 0.00057762265)
       ))    



(defmethod! transratio ((midicent list))
   (mapcar 'transratio midicent))


(defmethod! om-scale-exp ((self list) (minout number) (maxout number) (exponent number) &optional (minin 0) (maxin 0))
  :initvals '(1 0 1 1) 
  :indoc '("number or list"  "a number" "a number" "an exponent")
  :icon '(209)
  :doc 
  "Scales <self> (a number or list of numbers) considered to be in the interval [<minin> <maxin>] towards the interval [<minout> <maxout>].

If [<minin> <maxin>] not specified or equal to [0 0], it is bound to the min and the max of the list.

Ex. (om-scale 5 0 100 0 10)  => 50
Ex. (om-scale '(0 2 5) 0 100 0 10)  => (0 20 50)
Ex. (om-scale '(0 2 5) 0 100)  => (0 40 100)
 "
  (om-scale (om^ (om-scale self 0. 1. minin maxin) exponent) minout maxout 0. 1.)
  )

(defmethod! om-scale-exp ((self number) (minout number) (maxout number) (exponent number) &optional (minin 0) (maxin 0))
  (om-scale (om^ (om-scale self 0. 1. minin maxin) exponent) minout maxout 0. 1.)
  )

#|
;why doesnt this work?
(defun create-window (nothing)
  (make-instance 'Gen20
                 :x-points (list 0 4096)
                 :y-points (list 2 2)
                 :decimals 10
                 :id "?"
                 :size 4097
                 ))
|#

(defmethod! gen-window ((window t) &key (id "?") (maxamp 1.0) (size 4097) (decimals 10) (params 1))
            :icon '(209)
            :initvals '("Triangle" "?" 1.0 4097 10 1)
            :menuins '((0 (("Hamming" "Hamming") ("Hanning" "Hanning") ("Bartlett" "Bartlett") ("Blackman" "Blackman")
                           ("Blackman-Harris" "Blackman-Harris") ("Gaussian" "Gaussian") ("Kaiser" "Kaiser") ("Rectangle" "Rectangle") 
                           ("Sync" "Sync") ("Triangle" "Triangle") ("Line" "Line"))))
            ;note: currently maxamp isn't implemented for every window... later GEN20 will be changed to GEN-20
            (cond    
             ((or (equal window "Hamming") (equal window 1))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(1 0) decimals id size))
             ((or (equal window "Hanning") (equal window 2))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(2 0) decimals id size))
             ((or (equal window "Bartlett") (equal window 3))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(3 0) decimals id size))
             ((or (equal window "Blackman") (equal window 4))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(4 0) decimals id size))
             ((or (equal window "Blackman-Harris") (equal window 5))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(5 0) decimals id size))
             ((or (equal window "Gaussian") (equal window 6))
              (make-cs-table 'Gen20  (list 0 (1- size)) (list 6 params) decimals id size))

;all these below here don't work...t why?
             ((or (equal window "Kaiser") (equal window 7))
              (make-cs-table 'Gen20  (list 0 (1- size)) (list 7 (* 10 params)) decimals id size))
             ((or (equal window "Rectangle") (equal window 8))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(8 0) decimals id size))
             ((or (equal window "Sync") (equal window 9))
              (make-cs-table 'Gen20  (list 0 (1- size)) '(9 0) decimals id size))
             ((or (equal window "Triangle") (equal window 10))
              (make-cs-table 'Gen-07  (list 0 (round (/ size 2)) (1- size)) (list 0 maxamp 0) decimals id size))
             ((or (equal window "Line") (equal window 11))
              (make-cs-table 'Gen-07  (list 0 (1- size)) (list maxamp maxamp) decimals id size))
             ))




(defmethod! sound->envelope ((self sound) (resolution number) &key (filtertype "lowpass") (windowsize 3) (recursions 5) (unit "linear"))
;based samples + samplerate  I can determine a bw in Hz (or samplerate of the Env-follower)
            :icon '(141) 
            :initvals '(nil nil "lowpass" 3 5)
            :menuins '((2 (("lowpass" "lowpass") ("mean" "mean") ("median" "median")))
                       (5 (("linear" "linear") ("dbfs" "dbfs"))))

            (let* ((thesamples (sound-points self resolution))
                   (sqrt-samples (mapcar (lambda (samples)
                                           (sqrt samples)) (om* thesamples thesamples))))
              (cond ((equal filtertype "lowpass")
                     (setf envelope (filtres::low-pass-rec sqrt-samples windowsize recursions)))
                    ((equal filtertype "mean")
                     (setf envelope (filtres::mean-filter-rec sqrt-samples windowsize recursions)))
                    ((equal filtertype "median")
                     (setf envelope (filtres::median-filter-rec sqrt-samples windowsize recursions))))
              (let* ((theenvelope
                     (if (equal unit "dbfs")
                         (lin->db envelope)
                       envelope))
                     (thedur (sound-dur self))
                    (thexpoints (arithm-ser 0 thedur (om/ thedur resolution) thedur)))
                (simple-bpf-from-list thexpoints theenvelope 'bpf 10)
                )))





;;; For now we need to re-define samples->sec function

#|
;;; SAMPLES / SECONDS
(defmethod! samples->sec ((samples number) &optional samplerate)
          :icon 141 
          :initvals '(0 nil)
          :indoc '("number of samples" "sample rate (Hz)")
          :numouts 1
          :doc "Converts <samples> to a time (or duration) in seconds depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the time."
          (float (/ samples (or samplerate *audio-sr*)))
          )

(defmethod! samples->sec ((samples list) &optional samplerate)
   (mapcar #'(lambda (input)
               (samples->sec input samplerate)) samples))

(defmethod! sec->samples ((secs number) &optional samplerate) 
          :icon 141  
          :initvals '(0 nil)
          :indoc '("duration (s)" "sample rate (Hz)")
          :numouts 1
          :doc "Converts <secs> to a number of samples depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the samples."
          (float (* secs (or samplerate *audio-sr*))))

(defmethod! sec->samples ((secs list) &optional samplerate) 
          (mapcar #'(lambda (input)
                      (sec->samples input samplerate)) secs)
          )
|#

#|
(defmethod! draw-searchspace ((self list) (searchspace number))
            :numouts 3
            (let* ((thelist  
                   (loop for bpf in self collect
                         (let* ((lowerpoints (y-offset bpf (* -1 searchspace)))
                                (upperpoints (y-offset bpf searchspace))
                                (xmin (list-min (first lowerpoints)))
                                (xmax (list-max (first lowerpoints)))
                                (ymin (list-min (second lowerpoints)))
                                (ymax (list-max (second upperpoints)))
                                )
                           
                           (list 
                            (simple-bpf-from-list (x-append (first upperpoints) (reverse (first lowerpoints))) (x-append (second upperpoints) (reverse (second lowerpoints))) 'bpc (decimals bpf))
                            (list xmin xmax)
                            (list ymin ymax))
                     )))
                   (thetranslist (mat-trans thelist)))
              (let ((xtrans (mat-trans (second thetranslist)))
                    (ytrans (mat-trans (third thetranslist))))
              (values (first thetranslist) 
                      (list
                       (list-min (first xtrans)) (list-max (second xtrans)))
                      (list
                       (list-min (first ytrans)) (list-max (second ytrans)))
                      )))
              )

(defmethod! draw-searchspaces ((markers list) (bpfs list) (searchparam list))
            (let ((theresult
                   (loop for marker in markers
                         for param in searchparam
                         collect
                         (multiple-value-list (draw-searchspaces marker bpfs param))))
                         )
              (let* ((thetranslist (mat-trans theresult))
                     (thex (mat-trans (second thetranslist)))
                     (they (mat-trans (third thetranslist))))               
              (values (flat (first thetranslist))
                      (list (list-min (first thex)) (list-max (second thex)))
                      (list (list-min (first they)) (list-max (second they))))
              )))

|#

(defmethod! bpf-colour ((bpf bpf) &key r g b)
            :icon '(402)
  (setf (bpfcolor bpf) (om-make-color (or r (om-random 0. 1.0))
                                      (or g (om-random 0. 1.0))
                                      (or b (om-random 0. 1.0))))
  bpf)

(defmethod! bpf-ran-colour ((bpf bpf) &key r g b)
                (bpf-colour bpf :r r :g g :b b))



(defmethod! bpf-colour ((bpf list) &key r g b)
   (let ((nbcolor (length bpf)))
     (loop for onebpf in bpf collect
           (bpf-colour onebpf :r r :g g :b b))
     ))


(defmethod! nvt-inspect ((self sdiffile) (descriptor string))
            ;:icon 501
            :icon 638
            (let* ((nvtlist (getnvtlist self))
                   (pairs
                    (flat (remove nil (loop for nvt in nvtlist collect
                                      (when (equal descriptor (find-in-nvt nvt "Tablename"))
                                        (nv-pairs nvt)))) 1))
                   (clpairs (remove nil
                                    (loop for pair in pairs collect
                                          (unless (equal (car pair) "TableName")
                                            pair)))))
              ;(flat clpairs 1)))
              clpairs))

;============== for copying files =================

#|
(defmethod! copydict ((sourcepath string) (target-dir string) &key)
            (om-copy-file)
            )
|#


;============== stupid hack to override 10 decimals limit for BPFs ==============

(defmethod check-decimals ((self bpf))
    (unless (and (integerp (decimals self))
                 (> (decimals self) 0) 
                 (<= (decimals self) 10))
    (cond ((not (integerp (decimals self)))
           (om-beep-msg "BPF decimals must be an integer value!")
           (setf (slot-value self 'decimals) 0))
          ((minusp (decimals self))
           (om-beep-msg "BPF decimals must be a positive integer!")
           (setf (slot-value self 'decimals) 0))
           )))

;==============

; Utility functions

(defun find-string-in-list (string list)
(position string list :test #'string-equal))
