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

; %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

; why does it require om-fil? and OMchroma? should work with class-arrays and without filters

(require-library "om-fil" t)
(require-library "OMChroma" t)

(compile&load (om-relative-path '("sources") "superclasses" ))

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
   
(recursive-load-classes (om-relative-path '("sources" "classes") nil) *current-lib*)
                 

(mapcar #'(lambda (file) (compile&load (om-relative-path '("sources") file )))
        '(
          "preferences"
          "file-io"
          "editors"
          "array-tools"
          "dispatch-cseq"
          "gabor"
          "fof"
          "soundgrain-decomp"
          "sgntv"
          "sgnct"
          "partials"
          "score-tools"
          "statistics"
          "utilities"
          "array-2-atoms"
          "make-dictionary"
          ))

(om::fill-library '(
                    ("Fof" (
                                  (nil nil nil (fof-params fof-decomp get-fof-params get-fof-array get-fof-array-resamp) nil)))
                    ("Gabor" (
                                  (nil nil nil (gabor-params gabor-decomp get-gabor-params get-gabor-array get-gabor-array-resamp) nil)))
                    ("Soundgrain" (
                                  (nil nil nil (sgn-params sgn-decomp get-sgn-params get-sgn-array get-sgn-array-resamp) nil)))
                    ("Soundgrain-labeled" (
                                  (nil nil nil (sgl-params sgl-decomp get-sgl-params get-sgl-array get-sgl-array-resamp) nil)))                  
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

;(setq *my-bg-pict* (om-load-and-store-picture "dibox" 'omato))


(print "
OM-Pursuit 0.1alpha
Marlon Schumacher 2013, CIRMMT/McGill University
pydbm by Graham Boyes
")