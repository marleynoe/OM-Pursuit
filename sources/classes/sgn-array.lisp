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

(defclass! sgn-array (soundgrain-array)
           (
            )
           (:icon 03)
           )


#| ; for now deactivated (or overwritten)
(defmethod objfromobjs ((self sgn-array) (type score-array))
  (let* ((sgndata (data self))
         (thepaths (loop for path in (filepath self) 
                         collect
                         (pathname-name path)))
         (thepitches (loop for name in thepaths
                           collect
                           (read-from-string
                           (my-string-until-char  
                            (second (multiple-value-list (my-string-until-char name "_")))
                            "-"))))
         (new (make-instance 'score-array
                             :numcols (length (first sgndata))
                             :onset (om-round (om* 1000 (first sgndata)))
                             :duration (om-round (om* 1000 (second sgndata)))
                             :midicent thepitches
                             :velocity (om-scale (sgn-amplitude (fourth sgndata) (third sgndata)) 1 127)
                             :channel (om+ (fifth sgndata) 1)
                             ;:velocity (om-scale-exp (sgn-amplitude (fourth sgndata) (third sgndata)) 5 127 .7);om-scale-exp might be better
                             )))
    new))
|#

(defmethod objfromobjs ((self sgn-array) (type score-array))
  (let* ((sgndata (data self))
         (thepaths (loop for path in (filepath self) 
                         collect
                         (pathname-name path)))
         (thepitches (loop for name in thepaths
                           collect
                           ;(read-from-string
                           (get-midicent name)))
        
         (thevelocities (loop for name in thepaths
                           collect
                           (get-velocity name)))

         (new (make-instance 'score-array
                             :numcols (length (first sgndata))
                             :onset (om-round (om* 1000 (first sgndata)))
                             :duration (om-round (om* 1000 (second sgndata)))
                             :midicent thepitches
                             :velocity (print (om* (om-scale-exp (sgn-amplitude (fourth sgndata) (third sgndata)) 0 2 0.5) thevelocities))
                             :channel (om+ (fifth sgndata) 1)
                             ;:velocity (om-scale-exp (sgn-amplitude (fourth sgndata) (third sgndata)) 5 127 .7);om-scale-exp might be better
                             )))
    new))

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