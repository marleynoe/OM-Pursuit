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


(defmethod! 3dc-scale ((self 3Dc) &key xmin xmax ymin ymax zmin zmax)  
           ; :initvals '(nil 0. 1. 0. 1. 0. 1.)
            (let (mybpf (theymin ymin) (theymax ymax) (thexmin xmin) (thexmax xmax) (thezmin zmin) (thezmax zmax))
              (unless (numberp ymin) (setf theymin (list-min (y-points self))))
              (unless (numberp ymax) (setf theymax (list-max (y-points self))))
              (unless (numberp xmin) (setf thexmin (list-min (x-points self))))
              (unless (numberp xmax) (setf thexmax (list-max (x-points self))))
              (unless (numberp zmin) (setf thezmin (list-min (z-points self))))
              (unless (numberp zmax) (setf thezmax (list-max (z-points self))))
              (setf mybpf (3dc-from-list (om-scale (x-points self) thexmin thexmax) (om-scale (y-points self) theymin theymax) (om-scale (z-points self) thezmin thezmax) '3dc (decimals self)))
              mybpf))

(defmethod! 3dc-scale ((self 3Dc-lib) &key  xmin xmax ymin ymax zmin zmax) 
            (mapcar #'(lambda (theobject) (3dc-scale theobject :ymin ymin :ymax ymax :xmin xmin :xmax xmax :zmin zmin :zmax zmax))
            (bpf-list self)))

(defmethod! bpf-ran-colour ((bpf bpf) &key r g b)
            :icon '(402)
  (setf (bpfcolor bpf) (om-make-color (or r (om-random 0. 1.0))
                                      (or g (om-random 0. 1.0))
                                      (or b (om-random 0. 1.0))))
  bpf)

(defmethod! bpf-ran-colour ((bpf list) &key r g b)
   (let ((nbcolor (length bpf)))
     (loop for onebpf in bpf
           for i = 1 then (+ i 1) collect
           (bpf-ran-colour onebpf :r r :g g :b b))
     ))