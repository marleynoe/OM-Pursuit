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

;%%%%%%%%%%% ARRAY->ATOMS

(defmethod! array->atoms ((self gabor-array))
            :icon '(141) 
            (let ((molecule (molecule self))
                  (onset (onset self))
                  (duration (duration self))
                  (frequency (frequency self))
                  (magnitude (magnitude self))
                  (phi (phi self)))
              (loop 
               for mo in molecule
               for o in onset
               for d in duration
               for f in frequency
               for m in magnitude
               for p in phi
               collect
               (make-instance 'gabor-atom 
                                            :molecule mo
                                            :onset o
                                            :duration d
                                            :frequency f
                                            :magnitude m
                                            :phi p
                                            )))
               )

(defmethod! array->atoms ((self fof-array))
            (let ((molecule (molecule self))
                  (onset (onset self))
                  (duration (duration self))
                  (frequency (frequency self))
                  (risetime (risetime self))
                  (decaytime (decaytime self))
                  (magnitude (magnitude self))
                  (phi (phi self)))
                  ;(norm (norm self)))
              (loop 
               for mo in molecule
               for o in onset
               for d in duration
               for f in frequency
               for r in risetime
               for dt in decaytime
               for m in magnitude
               for p in phi
               ;for n in norm
               collect
               (make-instance 'fof-atom 
                                            :molecule mo
                                            :onset o
                                            :duration d
                                            :frequency f
                                            :risetime r
                                            :decaytime dt
                                            :magnitude m
                                            :phi p
                                            ;:norm n
                                            )))
               )

(defmethod! array->atoms ((self sgl-array))
            :icon '(141) 
            (let ((molecule (molecule self))
                  (onset (onset self))
                  (duration (duration self))
                  (corpus-index (corpus-index self))
                  (file-index (file-index self))
                  (norm (norm self))
                  (pitch (pitch self))
                  (velocity (velocity self))
                  (magnitude (magnitude self))
                  (filepath (filepath self)))

              (loop 
               for mo in molecule
               for o in onset
               for d in duration
               for c in corpus-index
               for f in file-index
               for n in norm
               for p in pitch
               for v in velocity
               for m in magnitude
               for fp in filepath
               collect
               (make-instance 'sgl-atom 
                                            :molecule mo
                                            :onset o
                                            :duration d
                                            :corpus-index c
                                            :file-index f
                                            :norm n
                                            :pitch p
                                            :velocity v
                                            :magnitude m
                                            :filepath fp
                                            )))
               )