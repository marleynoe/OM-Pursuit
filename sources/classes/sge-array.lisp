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

(defclass! sge-array (soundgrain-array)
           (
            )
           (:icon 03)
           )

(defmethod objfromobjs ((self sge-array) (type score-array))
  (let* ((sgedata (data self))
         (thenames (loop for path in (filepath self) 
                         collect
                         (pathname-name path)))
         (themcvellist (mat-trans (get-sg-mcvel thenames)))
         (midicent (first themcvellist))
         (velocity (second themcvellist))
         (new (make-instance 'score-array
                             :numcols (length (first sgedata))
                             :midicent midicent
                             :onset (om-round (om* 1000 (first sgedata)))
                             :duration (om-round (om* 1000 (second sgedata)))
                             :velocity (om-scale (sge-amplitude (fourth sgedata) (third sgedata)) 1 127) ;velocity
                             )))
    new))

(defmethod! get-sg-mcvel ((filename string))
            (let* ((valuelist (multiple-value-list (split-string (second (multiple-value-list (split-string filename "_"))) "-")))
                   (midicent (string-to-number (first valuelist)))
                   (velocity (string-to-number (second valuelist))))
              (list midicent velocity)
              ))

(defmethod! get-sg-mcvel ((filename list))
            (mapcar (lambda (thefile)
                      (get-sg-mcvel thefile)) filename)
            )
  

; %%%%%%%%%%%% OBJFROMOBJS for Chroma classes
#|
(defmethod objfromobjs ((self sge-array) (type smpl-1))
  (let* ((sgedata (data self))
         (paths (seventh sgedata))
         (theamplitudes (om* 1000 (sge-amplitude (om* (fourth sgedata) 2.36) (third sgedata))))
         (new (make-instance 'smpl-1
                             :numcols (length (first sgedata))
                             :e-dels (first sgedata)
                             :durs (second sgedata)
                             :amp theamplitudes
                             :afil paths
                            ;:aenv (make-cs-table 'Gen20  '(0 4096) '(5 1) 1 "?" 4097) ;blackman-harris window
                             :aenv (make-cs-table 'Gen07  '(0 512) '(1000 1000) 1 "?" 513) ;straight line
                             :wrap 0
                             )))
    new))
|#
