;************************************************************************
; OM-Pursuit, library for dictionary-based sound modelling in OpenMusic *
;      (c) 2011-2013 Marlon Schumacher (CIRMMT/McGill University)       *     
;               https://github.com/marleynoe/OM-Pursuit                 *
;                                                                       *
;                DSP based on pydbm - (c) Graham Boyes                  *
;                  https://github.com/gboyes/pydbm                      *
;************************************************************************
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
;Authors: J.Bresson, M. Schumacher

(in-package :om)

  
(defmethod! mapgrain ((self soundgrain-matrix) matching-fun)
            :icon '(333)
            (compile-patch matching-fun)
            (loop for col from 0 to (1- (numcols self)) collect
                  (let* ((box (make-instance 'temporalbox))
                         (vals 
                          (multiple-value-list 
                           (funcall (intern (string (code matching-fun)) :om)
                                    (loop for slot in (get-all-initargs-of-class (type-of self)) collect
                                          (list (name slot)
                                                (get-array-val self (name slot) col))))))
                         (names (mapcar #'(lambda (out) 
                                            (intern (frame-name out) :om))
                                        (sort (find-class-boxes (boxes matching-fun) 'omout) '< :key 'indice)))
                         (slots (mat-trans (list names vals))))
                    
                    (setf (free-store box) vals)
                    
                    (loop for item in slots do
                          (if (is-om-slot? (type-of box) (car item))
                              
                              (set-slot box  (car item) (if (floatp (cadr item))
                                                            (om-round (cadr item))
                                                          (cadr item)))
                            (om-beep-msg (format nil "Error: slot ~A does not exist in class TemporalBox !" (car item))))
                          )
                    box
                    )))


;(defmethod! grainmap ((self maquette) 

(defmethod! soundgrain-slot (grain name)
            :icon '(335) ;'(333)
            
            :menuins '((1 (("onset" "onset") ("duration" "duration") ("amplitude" "amplitude") 
                            ("file-index" "file-index") ("corpus-index" "corpus-index") 
                            ("iteration-id" "iteration-id") ("filepath" "filepath"))))
            (cadr (find name grain :test 'string-equal :key 'car)))

(defmethod! sgn-slot (grain name)
            :icon '(335) ;'(333)
            
            :menuins '((1 (("onset" "onset") ("duration" "duration") ("magnitude" "magnitude") 
                           ("norm" "norm") ("corpus-index" "corpus-index") ("file-index" "file-index") ("filepath" "filepath"))))
            (cadr (find name grain :test 'string-equal :key 'car)))

; ps how is the color of a temporalbox set? Might be good for symbolic parameters (labels, etc.) or for spatialization data (panning value, etc.)
