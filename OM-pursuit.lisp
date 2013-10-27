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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


(defun recursive-load-classes (dir &optional pack)
  (loop for item in (om-directory dir) do
        (if (directoryp item) 
            (let ((thepackage 
                   (or (find (car (last (pathname-directory item)))
                             (subpackages pack) :key 'name :test 'string-equal)
                       pack)))
               (recursive-load-classes item thepackage))    
        (when (string-equal (pathname-type item) "lisp")
          (load item)
          (addclass2pack (intern (string-upcase (pathname-name item))) pack)))))
                 
(defparameter *om-pursuit-lib-path* (make-pathname :directory (pathname-directory *load-pathname*)))

(mapcar #'(lambda (file) (compile&load (om-relative-path '("sources" "classes") file)))
        '(
          "superclasses"
          "sgn-constraint"
          "score-array"
          "sgn-array"
          "soundgrain-matrix"
          ))

(mapcar #'(lambda (file) (compile&load (om-relative-path '("sources") file )))
        '(
          "preferences"         
          "file-io"
          "editors"
          "array-tools"
          "dispatch-cseq"
          "score-tools"
          "SDIF-tools"
          "ircamdescriptors"
          "statistics"
          "utilities"
          "make-dictionary"
          "constraints"
          ))




(om::fill-library '(
                    ("Soundgrain" (
                                  (nil nil nil (sgn-params sgn-decomp get-sgn-params get-sgn-array get-sgn-array-resamp) nil)))             
                    ("Array-tools" (
                                  ("array" nil nil (process-array array-vals array-rep-filter) nil)
                                  ("component" nil nil (process-array-comp get-comp-vals comp-quantize field-quantize comp-perturbation field-perturbation comp-bandfilter) nil)
                                  ("slot" nil nil (process-array-slot slot-lowpass slot-highpass slot-scale) nil)
                                  ("array-field" nil nil (process-array-field array-field field-lowpass field-highpass) nil))
                                 nil nil nil)
                    ("Utilities" (
                                  (nil nil nil (get-bpf-points atoms->chords ) nil)))                   
                    ))
         
;(sub-pack-name subpack-lists class-list function-list class-alias-list)


(defmethod get-fonde-pict ((self soundgrain-matrix)) *pursuit-bg-pict*)
(setq *pursuit-bg-pict* (om-load-and-store-picture "dibox" 'om-pursuit))

; Version control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; before committing
; (CL-User::clean-sources *om-pursuit-lib-path*)

; Distribution 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; generate html reference
; (gen-lib-reference "OM-Pursuit")

; before distribution
; (clean-repo *om-pursuit-lib-path*) 

; (set-lib-release 1.0) this doesn't work!

(defun clean-repo (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
             
              (if (system::directory-pathname-p file)
                  (if (cond (
                             (string-equal ".git" (car (last (pathname-directory file))))
                             (string-equal ".dropbox" (car (last (pathname-directory file))))
                             (string-equal ".svn" (car (last (pathname-directory file))))
                      (system::call-system (concatenate 'string "rm -Rf \"" (namestring file) "\""))
                    (clean-repo file))
                (when (and (pathname-type file)
                           (or (string-equal (pathname-type file) "xfasl")
                               (string-equal (pathname-type file) "fasl")
                               (string-equal (pathname-type file) "DS_STORE")
                               (string-equal (pathname-type file) "nfasl")
                               (string-equal (pathname-type file) "ofasl")
                               (string-equal (pathname-type file) "ufasl")
                               (string-equal (pathname-type file) "lisp~")))
                  (print (concatenate 'string "Deleting " (namestring file) " ..."))
                  (delete-file file)
                  )
                ))
          (directory (namestring src-root) :directories t))))
    ))

#|
; SPLASH SCREEN
(om-message-dialog 
"===========================
                  OM-Pursuit v1.0beta
     Dictionary-based Sound Modelling in OpenMusic

(c) 2011-2013, Marlon Schumacher (CIRMMT/McGill University)
         https://github.com/marleynoe/OM-Pursuit

          DSP based on pydbm by Graham Boyes 
           https://github.com/gboyes/pydbm

" 
:window-title "OM-Pursuit v1.0beta" 

:size (om-make-point 360 200) 
:position (om-make-point 200 140)
)
|#

(format *om-stream* "

   *************************************************
   *                  OM-PURSUIT                   *
   * Dictionary-based Sound Modelling in OpenMusic *
   *                                               *
   *       (c) 2011-2013 Marlon Schumacher         *
   *    https://github.com/marleynoe/OM-Pursuit    *
   *                                               *
   *     DSP based on pydbm - (c) Graham Boyes     *
   *        https://github.com/gboyes/pydbm        *
   *************************************************
")