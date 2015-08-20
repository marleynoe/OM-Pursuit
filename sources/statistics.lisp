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
;Authors: M. Schumacher, G.Boyes

(in-package :om)

;%%%%%%%%%% STATISTICAL FUNCTIONS %%%%%%%%%%%

(defun mean (list)
  (/ (om-sum list) (length list))
  )

#|
(defun std-dev (list)
  (let* ((themean (mean list))
         (thedifferences (om- themean list))
         (squaredif (om-square thedifferences))
         (thediffmean (mean squaredif)))
    (sqrt thediffmean)
    ))
|#

(defun std-dev (list)
  (sqrt (mean (om-square (om- (mean list) list)))
    ))

(defun std-dev2 (list)
  (let ((themean (mean list)))
  (* (/ (sqrt (mean (om-square (om- themean list)))) themean) 100)
    ))

(defun variance (list)
  (mean (om-square (om- (mean list) list))
    ))

(defun median-variance (list)
  (mean (om-square (om- (median list) list))
    ))

(defun median-std-dev (list)
  (sqrt (mean (om-square (om- (median list) list)))
    ))

(defun median-std-dev2 (list)
  (let ((themedian (median list)))
    (* (/ (sqrt (mean (om-square (om- themedian list)))) themedian) 100)
    ))

(defun median (list)
  (nth (round (/ (length list) 2)) (sort list '<)))

(defun om-float (input)
  (loop for item in input collect
        (float item)
        ))

(defun om-sqrt (list)
  (mapcar #'(lambda (theitems)
              (sqrt theitems)) list))

(defun om-square (list)
  (mapcar #'(lambda (theitems)
              (expt theitems 2)) list))


(defun covariance (scalars)
  (sqrt (/ (om-sum2 scalars) (length scalars)))
  )
              

(defmethod! om-sum ((self list))
                  (loop for item in self
                  sum item)
                  )
; squared sum
(defmethod! om-sum^2 ((self list))
                  (loop for item in self
                  sum (* item item))
                  )





