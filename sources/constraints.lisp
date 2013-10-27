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
;Authors: M. Schumacher, G.Boyes

(in-package :om)

(defvar *pursuit-constraint-sdif-types* 
  (x-append 
   (make-instance 'sdiftype
                  :struct 'f
                  :signature "XPCT"
                  :description '(("XPCT" "Pursuit-Constraint") ("XMSA" "Max-simultaneous-atoms") ("XMSC" "Max-simultaneous-corpus-atoms") ("XMNT" "Min-time-distance") ("XMXT" "Max-time-distance")) 
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XPCT"
                  :description '("Pursuit-Constraint")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMSA"
                  :description '("Max-simultaneous-atoms")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMSC"
                  :description '("Max-simultaneous-corpus-atoms")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMNT"
                  :description '("Min-time-distance")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMXT"
                  :description '("Max-time-distance")
                  )
   ))

(defvar *om-pursuit_default-nvt* (make-instance 'sdifnvt
                                                :nv-pairs '(("This file was produced by" "OM-Pursuit") ("Authors" "Marlon Schumacher, Graham Boyes"))
                                                :tablename "OM-Pursuit Info"
                                                :id 0 ))


(defmethod! sgn-constraint-p ((self t)) nil)
(defmethod! sgn-constraint-p ((self sgn-constraint)) t)

;function that will return an instance of class 'sgn-constraint
(defmethod! ctr-define ((constraint symbol) (descriptor string) (order integer) (value t))
            :icon  22 ;02 ;20 
            :initvals '(nil "0" 0 nil)
            :menuins (list (list 0 (list '("<" '<) '("<=" '<=) '(">" '>) '(">=" '>=)  '("==" '==) '("!=" '!=)  '("W" 'W) '("B" 'B) ))
                        ;(1 ( ("SpectralCentroid" "1SCN") ("SpectralKurtosis" "1SLK") ("SpectralRolloff" "1SLR") ("SpectralDecrease" "1SPD") )))
                        (list 1 *ircamdescriptortypes*))

            :indoc '("constraint as a symbol" "which descriptor to apply the constraint to" "an integer specifying the order" "the value for the constraint (static = number, dynamic = bpf)")
            :doc "Defines and returns an instance of sgn-constraint"
            (make-instance 'sgn-constraint
                           :constraint constraint
                           :descriptor descriptor
                           :order order
                           :value value)
            )


(defmethod! ctr-define ((constraint integer) (descriptor string) (order integer) (value t))
            (make-instance 'sgn-constraint
                           :constraint constraint
                           :descriptor descriptor
                           :order order
                           :value value)
            )

; this function allows combining different sub-constraints (constraint-blocks) into larger scale tree-structures
(defmethod! ctr-combine ((operator symbol) (constraint1 t) (constraint2 t))
            :icon 11
            :initvals '(nil nil nil)
            :menuins '( (0 (("AND" 'AND) ( "NAND" 'NAND ) ( "OR" 'OR ) ( "NOR" 'NOR ) ( "XOR" 'XOR ) ( "XNOR" 'XNOR ))))
            (list operator (list constraint1 "," constraint2))
            )

(defmethod! ctr-weight ((constraint t) (weighting t))
            :icon 15
            ;:indoc 
            ;:menuins '( (0 (( "W" 'W ) ( "B" 'B ))))
            ;(print (list operator constraint weighting))
            (if ;(and
                (or (equal (constraint weighting) 'w) (equal (constraint weighting) 'b))
                ;(not (or (equal (constraint constraint) 'w) (equal (constraint constraint) 'b)))
                ;)         
                ;now I can't specify NILs anymore (because of the check for 
                (list (constraint weighting) (list constraint "," weighting))
              ;  (format nil "(~a (~a,~a))" operator constraint weighting)
              (om-beep-msg "Please connect a constraint to left input and a weight to right input"))
              )
            
; this function takes a list of sgn-constraints and operators and writes an SDIF files
(defmethod! ctr-compile ((ctr-constraint t))
            :icon 10
            ;(print constraint)
            (let* ((object-list (flat (list-filter 'sgn-constraint-p (list! ctr-constraint) 'pass)))
                   (side-effect (loop for item in object-list 
                                      for i from 1 to (length object-list) do
                                      (setf (streamid item) i)
                                      )) 
                   (full-constraint (print (replace-ctr-with-str ctr-constraint)))
                   ;(print "thefullconstraint")
                   ;(print full-constraint)
                   (frame-list
                    (loop for item in object-list 
                          for i from 1 to (length object-list) collect
                                
                          (list
                           ;write StreamID table
                           (make-instance 'sdifsid
                                          :id i
                                          :source (string+ "constraint-" (number-to-string i))
                                          :treeway (format nil "~a ~a ~d ~d" (constraint item) (descriptor item) (order item) (streamid item))
                                          ;(format nil "~s ~s ~d ~d" (constraint item) (descriptor item) (order item) (streamid item))
                                          )
                           
                           ; write SDIF frames
                           (if (bpf-p (value item))
                               (loop for pair in (point-pairs (value item)) collect
                                     (make-instance 'sdifframe
                                                    :signature "XPCT"
                                                    :ftime (car pair)
                                                    :StreamId i
                                                    :LMatrix (make-instance 'raw-sdifmatrix
                                                                            :signature "XPCT"
                                                                            :data (list (second pair))
                                                                            )
                                                    )
                                     )
                             (make-instance 'sdifframe
                                            :signature "XPCT"
                                            :ftime 0
                                            :StreamId i
                                            :LMatrix  (make-instance 'raw-sdifmatrix
                                                                            :signature "XPCT"
                                                                            :data (list (value item))
                                                                            )
                                            )
                             ))
                          ))
                   
                   ; write nvt
                   (nvt (make-instance 'sdifnvt
                                       :nv-pairs (print (list (list "Fullconstraint" (replace-string (itemtostring full-constraint) " , "))))
                                       :tablename "Constraint"
                                       :id 0))
                   (transframes (mat-trans frame-list)))
              
              (make-instance 'sdif-buffer
                             :types *pursuit-constraint-sdif-types*
                             :nvts (x-append *om-pursuit_default-nvt* nvt (car transframes))
                             :lframes (flat (cdr transframes))
                             )   
            ))

#|
(defmethod! ctr-compile ((ctr-constraint sgn-constraint))
            (call-next-method)
            )
|#

(defmethod! ctr-compile-mp ((ctr-constraint t))
            :icon 10 ;02
            ;(print constraint)
            (let* ((object-list (flat (list-filter 'sgn-constraint-p (list! ctr-constraint) 'pass)))
                   (side-effect (loop for item in object-list 
                                      for i from 1 to (length object-list) do
                                      (setf (streamid item) i)
                                      )) 
                   (full-constraint (print (replace-ctr-with-str ctr-constraint)))
                   ;(print "thefullconstraint")
                   ;(print full-constraint)
                   (frame-list
                    (loop for item in object-list 
                          for i from 1 to (length object-list) collect
                                
                          (list
                           ;write StreamID table
                           (make-instance 'sdifsid
                                          :id i
                                          :source (string+ "constraint-" (number-to-string i))
                                          :treeway (format nil "~a ~a ~d ~d" (constraint item) (descriptor item) (order item) (streamid item))
                                          )
                           
                           ; write SDIF frames
                           (if (bpf-p (value item))
                               (loop for pair in (point-pairs (value item)) collect
                                     (make-instance 'sdifframe
                                                    :signature "XPCT"
                                                    :ftime (car pair)
                                                    :StreamId i
                                                    :LMatrix (make-instance 'raw-sdifmatrix
                                                                            :signature (descriptor item)
                                                                            :data (list (second pair))
                                                                            )
                                                    )
                                     )
                             (make-instance 'sdifframe
                                            :signature "XPCT"
                                            :ftime 0
                                            :StreamId i
                                            :LMatrix  (make-instance 'raw-sdifmatrix
                                                                            :signature (print (descriptor item))
                                                                            :data (list (value item))
                                                                            )
                                            )
                             ))
                          ))
                   
                   ; write nvt
                   (nvt (make-instance 'sdifnvt
                                       :nv-pairs (print (list (list "Fullconstraint" (replace-string (itemtostring full-constraint) " , "))))
                                       :tablename "Constraint"
                                       :id 0))
                   (transframes (mat-trans frame-list)))
              (make-instance 'sdif-buffer
                             :types *pursuit-constraint-sdif-types*
                             :nvts (x-append *om-pursuit_default-nvt* nvt (car transframes))
                             :lframes (flat (cdr transframes))
                             )   
            ))


#|
; why can't I set the signature of an sdifmatrix?
(signature (make-instance 'sdifmatrix
                          :numcols 2
                          :signature (print "XPCT")
                                                                         ;:par1 (value item)
                          ))
|#

; this function takes a constraint-tree and replaces the objects with values of the slots

(defmethod! replace-ctr-with-str ((self sgn-constraint))
            ;(format nil "( ~a ~a ~d ~d )" (constraint self) (descriptor self) (order self) (streamid self))
            (list (constraint self) (descriptor self) (order self) (streamid self))
            )

;what's this for?
(defmethod! replace-ctr-with-str ((self t))
            self
            )

(defmethod! replace-ctr-with-str ((self list))
            (mapcar #'(lambda (x) (replace-ctr-with-str x)) self)
            )

;; CHECK THESE FUNCTIONS FOR MORE EFFICIENT SDIF WRITING


;;;================================================================================================================
;;; BPF TO SDIF
;;;================================================================================================================

#|
(defmethod! bpf->sdif ((self bpf) ftype mtype &optional (scope 'time) (typedefs nil) (outfile "mybpf.sdif"))
  :icon 608
  :initvals '(nil "1FQ0" "1FQ0" 'time nil nil "mybpf.sdif")
  :indoc '("a BPF" "frame type (string)" "matrix type (string)" "x = time or elements" "custom types declaration" "output file")
  :menuins '((3 (("Time" 'time) ("Elements" 'elts))))
  :doc "Saves the contents of <self> (a BPF) as an SDIF file in <outfile>.

<ftype> and <mtype> allow to determine the SDIF type to enclose the data in (default = 1FQ0, i.e. fundamental frequency).
If these types are not standard, they must be declared and given as a list of SDIFType objects in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

<scope> allows to choose whether the x-dimension of the BPF should be considered as time (default) or as the elements in a single matrix.
"
  (let* ((error nil) time
         (out-path (cond ((stringp outfile) (outfile outfile))
                         ((pathnamep outfile) outfile)
                         (t (om-CHOOSE-new-FILE-DIALOG))))
         (file (sdif-open-file (om-path2cmdpath out-path) 1))
         (datatype 4))
    (sdif::SdifFWriteGeneralHeader file)
    (write-nvt-tables file (list (default-om-NVT)))
    (when typedefs (write-types-table file (list! typedefs)))
    (sdif::SdifFWriteAllASCIIChunks file)
    (if (equal scope 'time)
        (let* ((framesize (+ 32 (calc-pad datatype)))
               (valptr (om-make-pointer datatype)))
          (loop for time in (x-points self)
                for val in (y-points self)
                while (not error) do
                (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce time 'double-float))
                (sdif::SdifFWriteFrameHeader file)
                (om-write-ptr valptr 0 :float (coerce val 'single-float))
                (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype 1 1 valptr)
                )
          (om-free-pointer valptr))
      (let* ((framesize (+ 32 (calc-pad (* datatype (length (point-list self))))))
             (valptr (om-make-pointer (* datatype (length (point-list self))))))
        (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce 0.0 'double-float))
        (sdif::SdifFWriteFrameHeader file)
        (loop for elt in (x-points self)
              for val in (y-points self)
              for i = 0 then (+ i 1)
              while (not error) do
              (om-write-ptr valptr (* i datatype) :float (coerce val 'single-float)))
        (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype (length (point-list self)) 1 valptr)
        (om-free-pointer valptr))
      )
    (sdif-close-file file)
    (om-namestring out-path)
    ))


;;;================================================================================================================
;;; MARKERS TO SDIF
;;;================================================================================================================

(defmethod! markers->sdif ((self list) &optional (ftype "1MRK") (typedefs nil) (outfile "markers.sdif"))
  :icon 608
  :initvals '(nil "1MRK" nil "mybpf.sdif")
  :indoc '("onset list (s)" "SDIF frame type" "custom types declaration" "output file")
  :doc "Saves <self> (a list of onsets) as an SDIF file in <outfile>.

<ftype> allows to determine the SDIF frame type to use (default = 1MRK, the standard SDIF type for time markers).
If this type is not standard, it must be declared and given as an SDIFType object in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

"
  (let* ((error nil) time
         (out-path (cond ((stringp outfile) (outfile outfile))
                         ((pathnamep outfile) outfile)
                         (t (om-CHOOSE-new-FILE-DIALOG))))
         (file (sdif-open-file (om-path2cmdpath out-path) 1))
         (datatype 4))
    (sdif::SdifFWriteGeneralHeader file)
    (write-nvt-tables file (list (default-om-NVT)))
    (when typedefs (write-types-table file (list! typedefs)))
    (sdif::SdifFWriteAllASCIIChunks file)
    (let* ((framesize (+ 32 (calc-pad datatype))))
      (loop for time in self
            while (not error) do
            (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 0 0 (coerce time 'double-float))
            (sdif::SdifFWriteFrameHeader file)))
    (sdif-close-file file)
    (om-namestring out-path)
    ))

;;;================================================================================================================
;;; BPF TO SDIF
;;;================================================================================================================

(defmethod! bpf->sdif ((self bpf) ftype mtype &optional (scope 'time) (typedefs nil) (outfile "mybpf.sdif"))
  :icon 608
  :initvals '(nil "1FQ0" "1FQ0" 'time nil nil "mybpf.sdif")
  :indoc '("a BPF" "frame type (string)" "matrix type (string)" "x = time or elements" "custom types declaration" "output file")
  :menuins '((3 (("Time" 'time) ("Elements" 'elts))))
  :doc "Saves the contents of <self> (a BPF) as an SDIF file in <outfile>.

<ftype> and <mtype> allow to determine the SDIF type to enclose the data in (default = 1FQ0, i.e. fundamental frequency).
If these types are not standard, they must be declared and given as a list of SDIFType objects in <typedefs>

If <outfile> is just a filename (not a pathname) the file is written in the default OM 'out-files' folder.

<scope> allows to choose whether the x-dimension of the BPF should be considered as time (default) or as the elements in a single matrix.
"
  (let* ((error nil) time
         (out-path (cond ((stringp outfile) (outfile outfile))
                         ((pathnamep outfile) outfile)
                         (t (om-CHOOSE-new-FILE-DIALOG))))
         (file (sdif-open-file (om-path2cmdpath out-path) 1))
         (datatype 4))
    (sdif::SdifFWriteGeneralHeader file)
    (write-nvt-tables file (list (default-om-NVT)))
    (when typedefs (write-types-table file (list! typedefs)))
    (sdif::SdifFWriteAllASCIIChunks file)
    (if (equal scope 'time)
        (let* ((framesize (+ 32 (calc-pad datatype)))
               (valptr (om-make-pointer datatype)))
          (loop for time in (x-points self)
                for val in (y-points self)
                while (not error) do
                (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce time 'double-float))
                (sdif::SdifFWriteFrameHeader file)
                (om-write-ptr valptr 0 :float (coerce val 'single-float))
                (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype 1 1 valptr)
                )
          (om-free-pointer valptr))
      (let* ((framesize (+ 32 (calc-pad (* datatype (length (point-list self))))))
             (valptr (om-make-pointer (* datatype (length (point-list self))))))
        (sdif::SdifFSetCurrFrameHeader file (sdif::SdifStringToSignature ftype) framesize 1 0 (coerce 0.0 'double-float))
        (sdif::SdifFWriteFrameHeader file)
        (loop for elt in (x-points self)
              for val in (y-points self)
              for i = 0 then (+ i 1)
              while (not error) do
              (om-write-ptr valptr (* i datatype) :float (coerce val 'single-float)))
        (sdif::SdifFWriteMatrix file (sdif::SdifStringToSignature mtype) datatype (length (point-list self)) 1 valptr)
        (om-free-pointer valptr))
      )
    (sdif-close-file file)
    (om-namestring out-path)
    ))
|#
