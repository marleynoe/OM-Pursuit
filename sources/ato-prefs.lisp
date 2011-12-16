; OMsox, 06/2010 M.Schumacher (CIRMMT/McGill University) 
; library for audio conversions and (batch) processing based on
; SoX - SoundeXchange - the Swiss Army knife of audio manipulation
; http://sox.sourceforge.net/
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

; %%%%%%%%%%%%% GABOR DECOMP %%%%%%%%%%%%%%%%%

(defvar *GABOR-PATH* "path to gaborDecomp")

(add-external-pref-module 'gabor)

(defmethod get-external-name ((module (eql 'gabor))) "Gabor")
(defmethod get-external-icon ((module (eql 'gabor))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'gabor)) modulepref) (get-pref modulepref :gabor-path))
(defmethod set-external-module-path ((module (eql 'gabor)) modulepref path) 
  (set-pref modulepref :gabor-path path))

(defmethod get-external-def-vals ((module (eql 'gabor))) 
  (list :gabor-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("gabor")) :name "gabor")
        ))

(defmethod save-external-prefs ((module (eql 'gabor))) 
  `(:gabor-path ,(om-save-pathname *GABOR-PATH*)))


(defmethod put-external-preferences ((module (eql 'gabor)) moduleprefs)
  (when (get-pref moduleprefs :gabor-path)
    (setf *GABOR-PATH* (find-true-external (get-pref moduleprefs :gabor-path)))
    (when (probe-file *GABOR-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *gabor-path*)) t))))

(put-external-pref-values 'gabor)

; %%%%%%%%%%%%% FOF DECOMP %%%%%%%%%%%%%%%%%

(defvar *FOF-PATH* "path to FOFDecomp")

(add-external-pref-module 'fof)

(defmethod get-external-name ((module (eql 'fof))) "Fof")
(defmethod get-external-icon ((module (eql 'fof))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'fof)) modulepref) (get-pref modulepref :fof-path))
(defmethod set-external-module-path ((module (eql 'fof)) modulepref path) 
  (set-pref modulepref :fof-path path))

(defmethod get-external-def-vals ((module (eql 'fof))) 
  (list :fof-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("fof")) :name "fof")
        ))

(defmethod save-external-prefs ((module (eql 'fof))) 
  `(:fof-path ,(om-save-pathname *FOF-PATH*)))


(defmethod put-external-preferences ((module (eql 'fof)) moduleprefs)
  (when (get-pref moduleprefs :fof-path)
    (setf *FOF-PATH* (find-true-external (get-pref moduleprefs :fof-path)))
    (when (probe-file *FOF-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *fof-path*)) t))))

(put-external-pref-values 'fof)

; %%%%%%%%%%%%% SGL DECOMP %%%%%%%%%%%%%%%%%

(defvar *SGL-PATH* "path to SGLDecomp")

(add-external-pref-module 'sgl)

(defmethod get-external-name ((module (eql 'sgl))) "Sgl")
(defmethod get-external-icon ((module (eql 'sgl))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'sgl)) modulepref) (get-pref modulepref :sgl-path))
(defmethod set-external-module-path ((module (eql 'sgl)) modulepref path) 
  (set-pref modulepref :sgl-path path))

(defmethod get-external-def-vals ((module (eql 'sgl))) 
  (list :sgl-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("sgl")) :name "sgl")
        ))

(defmethod save-external-prefs ((module (eql 'sgl))) 
  `(:sgl-path ,(om-save-pathname *SGL-PATH*)))


(defmethod put-external-preferences ((module (eql 'sgl)) moduleprefs)
  (when (get-pref moduleprefs :sgl-path)
    (setf *SGL-PATH* (find-true-external (get-pref moduleprefs :sgl-path)))
    (when (probe-file *SGL-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sgl-path*)) t))))


(put-external-pref-values 'sgl)

; %%%%%%%%%%%%% SGN DECOMP %%%%%%%%%%%%%%%%%

(defvar *SGN-PATH* "path to SGNDecomp")

(add-external-pref-module 'sgn)

(defmethod get-external-name ((module (eql 'sgn))) "Sgn")
(defmethod get-external-icon ((module (eql 'sgn))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'sgn)) modulepref) (get-pref modulepref :sgn-path))
(defmethod set-external-module-path ((module (eql 'sgn)) modulepref path) 
  (set-pref modulepref :sgn-path path))

(defmethod get-external-def-vals ((module (eql 'sgn))) 
  (list :sgn-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("sgn")) :name "sgn")
        ))

(defmethod save-external-prefs ((module (eql 'sgn))) 
  `(:sgn-path ,(om-save-pathname *SGN-PATH*)))


(defmethod put-external-preferences ((module (eql 'sgn)) moduleprefs)
  (when (get-pref moduleprefs :sgn-path)
    (setf *SGN-PATH* (find-true-external (get-pref moduleprefs :sgn-path)))
    (when (probe-file *SGN-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sgn-path*)) t))))


(put-external-pref-values 'sgn)

; %%%%%%%%%%%%% SGE DECOMP %%%%%%%%%%%%%%%%%

(defvar *SGE-PATH* "path to SGEDecomp")

(add-external-pref-module 'sge)

(defmethod get-external-name ((module (eql 'sge))) "Sge")
(defmethod get-external-icon ((module (eql 'sge))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'sge)) modulepref) (get-pref modulepref :sge-path))
(defmethod set-external-module-path ((module (eql 'sge)) modulepref path) 
  (set-pref modulepref :sge-path path))

(defmethod get-external-def-vals ((module (eql 'sge))) 
  (list :sge-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("sge")) :name "sge")
        ))

(defmethod save-external-prefs ((module (eql 'sge))) 
  `(:sge-path ,(om-save-pathname *SGE-PATH*)))


(defmethod put-external-preferences ((module (eql 'sge)) moduleprefs)
  (when (get-pref moduleprefs :sge-path)
    (setf *SGE-PATH* (find-true-external (get-pref moduleprefs :sge-path)))
    (when (probe-file *SGE-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sge-path*)) t))))

(put-external-pref-values 'sge)


; %%%%%%%%%%%%% MDC MAKE-DICTIONARY-EXECUTABLE %%%%%%%%%%%%%%%%%

(defvar *MDC-PATH* "path to make-dictionary")

(add-external-pref-module 'mdc)

(defmethod get-external-name ((module (eql 'mdc))) "Mdc")
(defmethod get-external-icon ((module (eql 'mdc))) (and (exist-lib-p "OMato") (list 01 (exist-lib-p "OMato"))))

(defmethod get-external-module-path ((module (eql 'mdc)) modulepref) (get-pref modulepref :mdc-path))
(defmethod set-external-module-path ((module (eql 'mdc)) modulepref path) 
  (set-pref modulepref :mdc-path path))

(defmethod get-external-def-vals ((module (eql 'mdc))) 
  (list :mdc-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OMato"))) '("mdc")) :name "mdc")
        ))

(defmethod save-external-prefs ((module (eql 'mdc))) 
  `(:mdc-path ,(om-save-pathname *mdc-path*)))


(defmethod put-external-preferences ((module (eql 'mdc)) moduleprefs)
  (when (get-pref moduleprefs :mdc-path)
    (setf *MDC-PATH* (find-true-external (get-pref moduleprefs :mdc-path)))
    (when (probe-file *MDC-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *mdc-path*)) t))))

(put-external-pref-values 'mdc)