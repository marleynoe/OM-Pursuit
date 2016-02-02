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

(defclass! soundgrain-matrix (class-array)
           (
            (onset     :accessor onset     :initarg :onset     :initform nil)
            (duration  :accessor duration  :initarg :duration  :initform nil)                      
            (amplitude :accessor amplitude :initarg :amplitude :initform nil)
            (file-index  :accessor file-index  :initarg :file-index  :initform nil)
            (corpus-index :accessor corpus-index :initarg :corpus-index :initform nil)
            (iteration-id  :accessor iteration-id  :initarg :iteration-id  :initform nil)        
            (filepath  :accessor filepath :initarg :filepath :initform nil)
            )            
           (:icon 03)
           )

(defmethod! GetpursuitSIDlist ((self sdiffile))
   (GetpursuitSIDlist (filepathname self)))

(defmethod! GetpursuitSIDlist ((self pathname))
   (GetpursuitSIDlist (namestring self)))

(defmethod! GetpursuitSIDlist ((self string))
            :icon 639
            :indoc '("SDIF file")
            :initvals '(nil)
            :doc "Returns the list of Stream ID descriptions in the SID table of <self>.

Each entry is a list of (ID source treeway).
See http://sdif.sourceforge.net/standard/sdif-standard.html#Stream%20IDs%20Table 
"
  (let ((data nil)
        (ptrfile (sdif-open self)))
    (unless (null ptrfile) 
      (sdif::SdifFReadGeneralHeader ptrfile)
      (sdif::SdifFReadAllASCIIChunks ptrfile)
      (let* ((idtable (sdif::SdifFStreamIDTable ptrfile))
             (nbitem (sdif::SdifStreamIDTableGetNbData idtable)))
        (setf data 
              (loop for i from 1 to nbitem collect
                    (let ((sid (sdif::SdifStreamIDTableGetSID idtable i)))
                      (list (sdif::SdifStreamIDEntryGetSID sid)
                            (sdif::SdifStreamIDEntryGetSource sid)
                            (string-to-number (sdif::SdifStreamIDEntryGetTreeWay sid))
                            ))
                    ))
        )
      (sdif-close self ptrfile))
    data))

(defun get-sid-triplets (SIDlist)
  (loop for item in (list! SIDlist) collect
        (x-append (id item) (source item) (treeway item))
        )
  )


(defmethod! find-sid ((SID list) (streamids t))
            :icon 639
            :indoc '("a list of SDIFSID objects" "A streamID (number) or list of StreamIDs")
            :initvals '(nil "")
            :doc "Returns source/treeway for StreamIDs from a list of StreamIDtable <sdifsid> objects."
    (let* ((thetriplets (get-sid-triplets sid))
           (sourcesandtreeways
            (loop for item in (list! streamids) collect
                  (last-n (find item thetriplets :test 'eql :key 'car) 2))))
      sourcesandtreeways
      ))

(defmethod! pursuit-sids ((self Sdiffile) (id t))
            :icon 639
            :indoc '("a SDIFSID object" "A streamID")
            :initvals '(nil nil)
            :doc "Returns source/treeway for a list of StreamIDs in a StreamIDtable <sdifsid>."

            (let* ((thesidlist (GetpursuitSIDlist self))
                  (thetriplets 
                   (loop for item in (list! id) collect
                         ;(find item thesidlist :test 'eql :key 'car))))
                        (first-n (find item thesidlist :test 'eql :key 'car) 2))))
              thetriplets))

(defmethod! pursuit-sid-paths ((self Sdiffile) (id t))
            :icon 639
            :indoc '("a SDIFSID object" "A streamID")
            :initvals '(nil nil)
            :doc "Returns source/treeway for a list of StreamIDs in a StreamIDtable <sdifsid>."

            (let* ((thesidlist (GetpursuitSIDlist self))
                  (thetriplets 
                   (loop for item in (list! id) collect
                         (second (find item thesidlist :test 'eql :key 'car)))))
              thetriplets))

#|
(let* ((thesdiffile self)
       (thesidlist (GetpursuitSIDlist thesdiffile))
       (
|#

(defun pursuit-sdif-streams (sdiffile)
  (sort-list (remove-dup (mapcar (lambda (streams)
                                                     (first streams)) (sdifstreams sdiffile)) 'eq 1))
  )

; this function gets the model-data from an SDIFfile
(defmethod! get-pursuit-data ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 8
            :outdoc '("numatoms" "onset" "duration" "amplitude" "file-index" "corpus-index" "iteration-id" "filepath")
            
            (let* ((thestreams (pursuit-sdif-streams self)) ; the SIDs of the streams in the file
                   (thepaths (pursuit-sid-paths self thestreams)) ; the paths associated to the streams in the SID table
                   (theglobaldata 
                    (loop for stream in thestreams 
                          for path in thepaths collect
                          (x-append
                           stream                           
                           (flat (getsdifdata self stream "XGLB" "XDUR" 0 nil nil nil nil))
                           (om-round (flat (getsdifdata self stream "XGLB" "XCRP" 0 nil nil nil nil)))
                           path
                          )))
                   (thesgrdata
                    (loop for stream in thestreams collect
                           (mat-trans (reverse (multiple-value-list  
                                  (getsdifdata self stream "XSGR" "XSGR" 0 nil nil nil nil)))))
                          )
                   (themergeddata (sort-list (regroup-frames theglobaldata thesgrdata) :test '< :key 'fifth))
                   ;(themergeddata (regroup-frames theglobaldata thesgrdata))
              ; (file-index duration crp-id path onset amplitude magn. iteration-index)
                   (thetransdata (mat-trans themergeddata)))
              (values (length thetransdata)
                      (fifth thetransdata) ; onset
                      (second thetransdata) ; duration
                      (sixth thetransdata) ; amplitude
                      (first thetransdata) ; file-index
                      (third thetransdata) ; corpus-index
                      (om-round (nth 7 thetransdata)) ; iteration-index
                      (fourth thetransdata)
                      )))

(defmethod! get-pursuit-data-list ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("numatoms" "onset" "duration" "amplitude" "file-index" "corpus-index" "iteration-id" "filepath")
            
            (let* ((thestreams (pursuit-sdif-streams self)) ; the SIDs of the streams in the file
                   (thepaths (pursuit-sid-paths self thestreams)) ; the paths associated to the streams in the SID table
                   (theglobaldata 
                    (loop for stream in thestreams 
                          for path in thepaths collect
                          (x-append
                           stream                           
                           (flat (getsdifdata self stream "XGLB" "XDUR" 0 nil nil nil nil))
                           (om-round (flat (getsdifdata self stream "XGLB" "XCRP" 0 nil nil nil nil)))
                           path
                          )))
                   (thesgrdata
                    (loop for stream in thestreams collect
                           (mat-trans (reverse (multiple-value-list  
                                  (getsdifdata self stream "XSGR" "XSGR" 0 nil nil nil nil)))))
                          )
                   (themergeddata (sort-list (regroup-frames theglobaldata thesgrdata) :test '< :key 'fifth))
                   (thetransdata (mat-trans themergeddata)))
              thetransdata ;note, this is 'unsorted', thus: (file-index duration crp-id path onset amplitude magn. iteration-index)
              ))


(defun regroup-frames (glblist sgrlist)
  (flat (loop for glbitem in glblist
        for sgritem in sgrlist collect
        (loop for subsgritem in sgritem collect
              (flat (x-append glbitem subsgritem))
              )
        ) 1)
  )

(defmethod! model-to-matrix ((self sdiffile))
            :icon 04
            :numouts 1
            :outdoc '("soundgrain-matrix")
            :doc "Converts an OM-Pursuit model represented in an SDIF into a soundgrain-matrix class"
            
            (let ((soundgrain-datalist (get-pursuit-data-list self)))
              (make-instance 'soundgrain-matrix
                             :numcols (length (first soundgrain-datalist))
                             :onset (fifth soundgrain-datalist)
                             :duration (second soundgrain-datalist)
                             :amplitude (sixth soundgrain-datalist)
                             :file-index (first soundgrain-datalist)
                             :corpus-index (third soundgrain-datalist)
                             :iteration-id (om-round (nth 7 soundgrain-datalist))
                             :filepath (fourth soundgrain-datalist))
                             )
              )


(defmethod objfromobjs ((self sdiffile) (type soundgrain-matrix))
  (model-to-matrix self)
  )

#|
(defmethod objfromobjs ((self soundgrain-matrix) (type smpl-1))
  (make-instance 'smpl-1
                 :numcols (numcols self)
                 :e-dels (onset self)
                 :durs (duration self)
                 :amp (print (om* (om-abs (amplitude self)) 1000))
                 :afil (filepath self)
                 :aenv (simple-bpf-from-list '(0 100) '(1000 1000) 'bpf 10)
                 )
  )
|#

;this is very slow !!! WHY??
(defmethod! get-descriptor-data ((self sdiffile) (streamnumber number) (descriptor string))
            :icon 639
            :indoc '("nothing" "nothing" "nothing")
            :initvals '(nil nil nil)
            (if (find descriptor '("XDUR" "XCRP" "XMDC" "XVEL") :test 'equal)
                (flat (getsdifdata self streamnumber "XGLB" descriptor nil nil nil nil nil))
               (flat (getsdifdata self streamnumber "1WMN" descriptor nil nil nil nil nil))
              ))

(defmethod! get-descriptor-data ((self sdiffile) (streamnumber list) (descriptor string))
            (flat (mapcar (lambda (thestreams)
                      (get-descriptor-data self thestreams descriptor)) streamnumber))
            )

#|      
(defmethod! modify-slot ((self soundgrain-matrix) (slot symbol) (value t))
            :icon 335
            (let* ((modifmatrix (clone self)))
              (set-slot modifmatrix slot value)
              (make-instance 'soundgrain-matrix
                             :numcols (numcols modifmatrix)
                             :onset (onset modifmatrix)
                             :durations (duration modifmatrix)
                             :amplitude (amplitude modifmatrix)
                             :file-index (file-index modifmatrix)
                             :corpus-index (corpus-index modifmatrix)
                             :iteration-id (iteration-id modifmatrix)
                             :filepath (filepath modifmatrix)
                             ))
            )
|#

(defmethod! modify-slot ((self class-array) (slot symbol) (value t))
            :icon '(335)
            (let ((modifarray (clone self)))
              (set-slot modifarray slot value)
              (set-data modifarray)
              modifarray)
            )

(defmethod! find-sid ((SID list) (streamids t))
            :icon 639
            :indoc '("a list of SDIFSID objects" "A streamID (number) or list of StreamIDs")
            :initvals '(nil "")
            :doc "Returns source/treeway for StreamIDs from a list of StreamIDtable <sdifsid> objects."
    (let* ((thetriplets (get-sid-triplets sid))
           (sourcesandtreeways
            (loop for item in (list! streamids) collect
                  (last-n (find item thetriplets :test 'eql :key 'car) 2))))
      sourcesandtreeways
      ))

;needs a loop in a loop (either with a push or with a collect)
#|
(setf list1 '(a b c d))
(setf list2 (list (list 1 2) (list (list (list 3 7) (list 4 9)) (list 5 6))))

(loop for item1 in list1
      for item2 in list2 collect
      (loop for item3 in (list! item2) collect
            (x-append item1 item3))
      )


(defun regroup-frames (glblist sgrlist)
  (flat (loop for glbitem in glblist
        for sgritem in sgrlist collect
        (loop for subsgritem in sgritem collect
              (flat (x-append glbitem subsgritem))
              )
        ) 1)
  )

|#

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


; convert directly into chord-seq

#|
;needs to be other way round: (self chord-seq) (type sgn-array)
(defmethod objfromobjs ((self sgn-array) (type chord-seq))
  (let* ((sgndata (data self))
         (thepaths (loop for path in (filepath self) 
                         collect
                         (pathname-name path)))
         (thepitches (loop for name in thepaths
                           collect
                           ;(read-from-string
                        (string-to-number 
                          (second (multiple-value-list (my-string-until-char (second (multiple-value-list (my-string-until-char (second (multiple-value-list (my-string-until-char (my-string-until-char name "-") "_"))) "_"))) "_"))))))
         
         (thevelocities (loop for name in thepaths
                           collect
                           (read-from-string
                           (string-to-number 
                            (my-string-until-char (second (multiple-value-list (my-string-until-char name "-")))"_")))))

         (new (make-instance 'chord-seq
                           ;  :numcols (length (first sgndata))
                             :lonset (om-round (om* 1000 (first sgndata)))
                             :ldur (om-round (om* 1000 (second sgndata)))
                             :lmidic thepitches
                             :lvel (print (om* (om-scale-exp (sgn-amplitude (fourth sgndata) (third sgndata)) 0 2 0.5) thevelocities))
                             :lchan (om+ (fifth sgndata) 1)
                             ;:velocity (om-scale-exp (sgn-amplitude (fourth sgndata) (third sgndata)) 5 127 .7); om-scale-exp might be better
                             )))
    new))
|#


; %%%%%%%%%%%% OBJFROMOBJS for Chroma classes
#|
(defmethod objfromobjs ((self sgn-array) (type smpl-1))
  (let* ((sgndata (data self))
         (paths (seventh sgndata))
         (theamplitudes (om* 1000 (sgn-amplitude (om* (fourth sgndata) 2.36) (third sgndata))))
         (new (make-instance 'smpl-1
                             :numcols (length (first sgndata))
                             :e-dels (first sgndata)
                             :durs (second sgndata)
                             :amp theamplitudes
                             :afil paths
                            ;:aenv (make-cs-table 'Gen20  '(0 4096) '(5 1) 1 "?" 4097) ;blackman-harris window
                             :aenv (make-cs-table 'Gen07  '(0 512) '(1000 1000) 1 "?" 513) ;straight line
                             :wrap 0
                             )))
    new))
|#


#|

; new methods for Prisma classes

(defmethod objfromobjs ((self sgn-array) (type richard))
  (let* ((sgndata (data self))
         (paths (seventh sgndata))
         (theamplitudes (sgn-amplitude (om* (fourth sgndata) 2.36) (third sgndata))); (third sgndata)); (sgn-amplitude (fourth sgndata) (third sgndata)))
         (new (set-array 'richard 
                         (length (car sgndata))
                         (list
                          'e-dels (first sgndata)
                          'durs (second sgndata)
                          'gain theamplitudes
                          'soundfile paths
                          'freqdev 1
                          'freqenv (make-cs-table 'Gen-07  '(0 256 512) '(100 50 0) 1 "?" 513)
                          'wrap 0
                          'freqjit-aenv (make-cs-table 'Gen-07  '(0 256 512) '(0 50 0) 1 "?" 513)
                          'freqjit-fenv (make-cs-table 'Gen-07  '(0 256 512) '(0 100 1000) 1 "?" 513)
                          ))))
    new))

|#