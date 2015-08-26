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


; %%%%%%%%%%%%% Soundgrain-decomp %%%%%%%%%%%%%%%%%

(defvar *om-pursuit-path* "path to om-pursuit-executable")

(add-external-pref-module 'om-pursuit)

(defmethod get-external-name ((module (eql 'om-pursuit))) "OM-Pursuit")
(defmethod get-external-icon ((module (eql 'om-pursuit))) (and (exist-lib-p "OM-Pursuit") (list 01 (exist-lib-p "OM-Pursuit"))))

(defmethod get-external-module-path ((module (eql 'om-pursuit)) modulepref) (get-pref modulepref :om-pursuit-path))
(defmethod set-external-module-path ((module (eql 'om-pursuit)) modulepref path) 
  (set-pref modulepref :om-pursuit-path path))

(defmethod get-external-def-vals ((module (eql 'om-pursuit))) 
  (list :om-pursuit-path (print (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executable")
                                            '("OM-Pursuit.app") '("Contents") '("MacOS")) :name "OM-Pursuit")
        )))

;(om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executable")) :name "OM-Pursuit-exec")


(defmethod save-external-prefs ((module (eql 'om-pursuit))) 
  `(:om-pursuit-path ,(om-save-pathname *om-pursuit-path*)))

(defmethod put-external-preferences ((module (eql 'om-pursuit)) moduleprefs)
  (when (get-pref moduleprefs :om-pursuit-path)
    (setf *om-pursuit-path* (find-true-external (get-pref moduleprefs :om-pursuit-path)))
    (when (probe-file *om-pursuit-path*)
      (om-cmd-line (format nil "chmod -R 777 ~s" (namestring *om-pursuit-path*)) t))))

(put-external-pref-values 'om-pursuit)

; %%%%%%%%%%%%% Soundgrain-decomp %%%%%%%%%%%%%%%%%

(defvar *SGN-PATH* "path to SGNDecomp")
#|
(add-external-pref-module 'sgn)

(defmethod get-external-name ((module (eql 'sgn))) "Sgn")
(defmethod get-external-icon ((module (eql 'sgn))) (and (exist-lib-p "OM-Pursuit") (list 01 (exist-lib-p "OM-Pursuit"))))

(defmethod get-external-module-path ((module (eql 'sgn)) modulepref) (get-pref modulepref :sgn-path))
(defmethod set-external-module-path ((module (eql 'sgn)) modulepref path) 
  (set-pref modulepref :sgn-path path))

(defmethod get-external-def-vals ((module (eql 'sgn))) 
  (list :sgn-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executables")) :name "sgn")
        ))

(defmethod save-external-prefs ((module (eql 'sgn))) 
  `(:sgn-path ,(om-save-pathname *SGN-PATH*)))


(defmethod put-external-preferences ((module (eql 'sgn)) moduleprefs)
  (when (get-pref moduleprefs :sgn-path)
    (setf *SGN-PATH* (find-true-external (get-pref moduleprefs :sgn-path)))
    (when (probe-file *SGN-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sgn-path*)) t))))

(put-external-pref-values 'sgn)



; %%%%%%%%%%%%% SGNTV DECOMP %%%%%%%%%%%%%%%%%

(defvar *SGNTV-PATH* "path to SGNTV-Decomp")

(add-external-pref-module 'sgntv)

(defmethod get-external-name ((module (eql 'sgntv))) "Sgntv")
(defmethod get-external-icon ((module (eql 'sgntv))) (and (exist-lib-p "OM-Pursuit") (list 01 (exist-lib-p "OM-Pursuit"))))

(defmethod get-external-module-path ((module (eql 'sgntv)) modulepref) (get-pref modulepref :sgntv-path))
(defmethod set-external-module-path ((module (eql 'sgntv)) modulepref path) 
  (set-pref modulepref :sgntv-path path))

(defmethod get-external-def-vals ((module (eql 'sgntv))) 
  (list :sgntv-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executables")) :name "sgntv")
        ))

(defmethod save-external-prefs ((module (eql 'sgntv))) 
  `(:sgntv-path ,(om-save-pathname *SGNTV-PATH*)))


(defmethod put-external-preferences ((module (eql 'sgntv)) moduleprefs)
  (when (get-pref moduleprefs :sgntv-path)
    (setf *SGNTV-PATH* (find-true-external (get-pref moduleprefs :sgntv-path)))
    (when (probe-file *SGNTV-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sgntv-path*)) t))))

(put-external-pref-values 'sgntv)



; %%%%%%%%%%%%% SGNCT DECOMP %%%%%%%%%%%%%%%%%

(defvar *SGNCT-PATH* "path to SGNCT-Decomp")

(add-external-pref-module 'sgnct)

(defmethod get-external-name ((module (eql 'sgnct))) "Sgnct")
(defmethod get-external-icon ((module (eql 'sgnct))) (and (exist-lib-p "OM-Pursuit") (list 01 (exist-lib-p "OM-Pursuit"))))

(defmethod get-external-module-path ((module (eql 'sgnct)) modulepref) (get-pref modulepref :sgnct-path))
(defmethod set-external-module-path ((module (eql 'sgnct)) modulepref path) 
  (set-pref modulepref :sgnct-path path))

(defmethod get-external-def-vals ((module (eql 'sgnct))) 
  (list :sgnct-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executables")) :name "sgnct")
        ))

(defmethod save-external-prefs ((module (eql 'sgnct))) 
  `(:sgnct-path ,(om-save-pathname *SGNCT-PATH*)))


(defmethod put-external-preferences ((module (eql 'sgnct)) moduleprefs)
  (when (get-pref moduleprefs :sgnct-path)
    (setf *SGNCT-PATH* (find-true-external (get-pref moduleprefs :sgnct-path)))
    (when (probe-file *SGNCT-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *sgnct-path*)) t))))

(put-external-pref-values 'sgnct)

; %%%%%%%%%%%%% MDC MAKE-DICTIONARY-EXECUTABLE %%%%%%%%%%%%%%%%%

(defvar *MDC-PATH* "path to make-dictionary")

(add-external-pref-module 'mdc)

(defmethod get-external-name ((module (eql 'mdc))) "Mdc")
(defmethod get-external-icon ((module (eql 'mdc))) (and (exist-lib-p "OM-Pursuit") (list 01 (exist-lib-p "OM-Pursuit"))))

(defmethod get-external-module-path ((module (eql 'mdc)) modulepref) (get-pref modulepref :mdc-path))
(defmethod set-external-module-path ((module (eql 'mdc)) modulepref path) 
  (set-pref modulepref :mdc-path path))

(defmethod get-external-def-vals ((module (eql 'mdc))) 
  (list :mdc-path (om-make-pathname :directory (append (pathname-directory (lib-pathname (exist-lib-p "OM-Pursuit"))) '("executables")) :name "mdc")
        ))

(defmethod save-external-prefs ((module (eql 'mdc))) 
  `(:mdc-path ,(om-save-pathname *mdc-path*)))


(defmethod put-external-preferences ((module (eql 'mdc)) moduleprefs)
  (when (get-pref moduleprefs :mdc-path)
    (setf *MDC-PATH* (find-true-external (get-pref moduleprefs :mdc-path)))
    (when (probe-file *MDC-PATH*)
      (om-cmd-line (format nil "chmod 777 ~s" (namestring *mdc-path*)) t))))

(put-external-pref-values 'mdc)

|#