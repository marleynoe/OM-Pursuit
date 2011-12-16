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

(defclass! sgl-array (soundgrain-array)
           (
            (pitch :accessor pitch :initarg :pitch :initform nil)
            (velocity :accessor velocity :initarg :velocity :initform nil)         
            (molecule     :accessor molecule     :initarg :molecule     :initform nil)
            )
           (:icon 03)
           )


; %%%%%%%% OBJFROMOBJS for Chroma classes

(defmethod objfromobjs ((self sgl-array) (type smpl-1))
  (let* ((sgldata (data self))
         (theamplitude (sgl-amplitude (fourth sgldata) (third sgldata)))
         (paths (seventh sgldata))
         (thevals (loop for path in paths
                             collect
                              (second (multiple-value-list (my-string-until-last-char (pathname-name path) "_")))))
         (thepitches (loop for item in thevals
                             collect
                             (read-from-string 
                              (my-string-until-char item "-"))))         
         (thevelocities (loop for item in thevals
                             collect
                             (read-from-string (second (multiple-value-list
                              (my-string-until-char item "-"))))))
         (new (make-instance 'smpl-1
                             :numcols (length (first sgldata))
                             :e-dels (first sgldata)
                             :durs (second sgldata)
                          ;   :amp (nth 8 sgldata); (om* theamplitude (om/ (om+ 1 (nth 8 sgldata)) (om+ 1 thevelocities)))
                             :f0 (transratio (om- (nth 7 sgldata) thepitches))
                             :afil paths
                            ;:aenv (make-cs-table 'Gen20  '(0 4096) '(5 1) 1 "?" 4097) ;blackman-harris window
                             :aenv (make-cs-table 'Gen07  '(0 512) '(1000 1000) 1 "?" 513) ;straight line
                             :wrap 0
                             )))
    new))

; taken from score-array to avoid startup errors

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

#| new methods for Prisma synthesizers

(defmethod objfromobjs ((self sgl-array) (type richard))
  (let* ((sgldata (data self))
         (theamplitude (sgl-amplitude (fourth sgldata) (third sgldata)))
         (paths (seventh sgldata))
         (thevals (loop for path in paths
                             collect
                              (second (multiple-value-list (my-string-until-last-char (pathname-name path) "_")))))
         (thepitches (loop for item in thevals
                             collect
                             (read-from-string 
                              (my-string-until-char item "-"))))         
         (thevelocities (loop for item in thevals
                             collect
                             (read-from-string (second (multiple-value-list
                              (my-string-until-char item "-"))))))
         (new (set-array 'richard 
                         (length (car sgldata))
                         (list
                          'e-dels (first sgldata)
                          'durs (second sgldata)
                         ; 'gain (om* theamplitude (om/ (om+ 1 (nth 8 sgldata)) (om+ 1 thevelocities)))
                          'soundfile paths
                          'freq (transratio (om- (nth 7 sgldata) thepitches))
                          'freqdev 0
                          'wrap 0
                          'freqjit-aenv (make-cs-table 'Gen-07  '(0 512) '(0 0) 1 "?" 513)
                          ;'freqjit-aenv (make-cs-table 'Gen-07  '(0 256 512) '(10 50 800) 1 "?" 513)
                          ;'freqjit-fenv (make-cs-table 'Gen-07  '(0 256 512) '(100 30 100) 1 "?" 513)
                          ;KEYWORDS
                          'pitch (nth 7 sgldata)
                          'velocity (nth 8 sgldata)
                          'molecule (nth 9 sgldata)
                          ))))
    new))

|#