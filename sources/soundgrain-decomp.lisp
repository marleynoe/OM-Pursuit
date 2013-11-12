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

; ---------------------------

(defclass LockedBoxCall (OMBoxCall) ())
(defmethod get-boxcallclass-fun ((self (eql 'soundgrain-decomp))) 'LockedBoxCall)
(defmethod initialize-instance :before ((self LockedBoxCall) &rest args)
(setf (om::allow-lock self) "&")) ; for eval-once-mode replace the "x" with a "&", for lambda "l"


(defmethod! soundgrain-params (&key (markers '((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2))) (corpora t)  
                               (max-atoms 5) (min-deviation 0.001))
                        :icon 04
                        :initvals '(((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2)) nil 5 0.001)
                        :indoc '("markers (sec)" "pathnames/strings pointing to directories containing soundfiles"  "max number of atoms" "min-deviation")
                        (let* ((themarkers (if (sound-p markers)
                                               (markers->frames markers)
                                             markers))
                               (thestring (format nil "\"~a\" '~a' ~d ~d" 
                                                  (python-directories corpora) 
                                                  themarkers
                                                  max-atoms min-deviation)))
                          thestring))

;(target_path, dictionary_path, cmax, SRR, maxsimul, mindistance, mod_outpath, res_outpath, sdif_outpath)

(defmethod! soundgrain-decomp ((target sound) (markers sdiffile) (dictionary sdiffile) (constraints sdiffile) (mp-constraints sdiffile) (max-iterations number) &key outpath downsampling-factor)
            (soundgrain-decomp (sound-path target) (filepathname markers) (filepathname dictionary) (filepathname constraints) (filepathname mp-constraints) max-iterations :outpath outpath
                               :downsampling-factor downsampling-factor))

(defmethod! soundgrain-decomp ((target pathname) (markers pathname) (dictionary pathname) (constraints pathname) (mp-constraints pathname) (max-iterations number) &key outpath (downsampling-factor 1.0))

;dictPath, constraintPath, targetPath, markerPath, mpconstrPath, modPath, resPath, sdifPath, maxiter

            :icon 04
            :numouts 3
            :initvals '(nil nil nil nil nil nil nil nil 1.0)
            :indoc '("target sound" "markers" "dictionary" "constraints" "mp-constraints" "maxiterations" "outpath" "downsampling factor")
            
            :outdoc '("model audio" "residual audio" "model sdif file")

            (print downsampling-factor)
            (if (probe-file *om-pursuit-path*)
                (let* ((inpath target)
                       (filename (pathname-name inpath))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a" filename)))
                       (outdir (or outpath (om-make-pathname :directory (append (pathname-directory inpath) (list name)))))                      
                       (sdif-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                       :name (string+ name "_ompursuit_model") :type "sdif"))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ name "__ompursuit_model") :type "wav"))
                       (residual-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ name "_ompursuit_residual") :type "wav"))
                       )
                  (om-create-directory outdir)
                  (setf str 
                        (format nil "~s ~s ~s ~s ~s ~s ~s ~s ~s ~d --dsf ~f" 
                                ;dictPath, constraintPath, targetPath, markerPath, mpconstrPath, modPath, resPath, sdifPath, maxiter
                                
                                (namestring *om-pursuit-path*)
                                (namestring dictionary)
                                (namestring constraints)
                                (namestring inpath)
                                (namestring markers)
                                (namestring mp-constraints)
                                (namestring audio-outfile)
                                (namestring residual-outfile)
                                (namestring sdif-outfile)
                                max-iterations
                                downsampling-factor
                                ))
                  (print str)
                  ;(print outdir)
                  ;(print sdif-outfile)
                  ;(print audio-outfile)
                  ;(print residual-outfile)
                  (om-cmd-line str *sys-console*)
                  ;(sys:run-shell-command str :wait nil) ;perhaps I can route this into the listener
                  (values (probe-file sdif-outfile) (probe-file audio-outfile) (probe-file residual-outfile))           
                  )
              (progn 
                (print "OM-Pursuit executable not found... set in preferences?")
                nil
                ))
            )

(defmethod! get-sgnct-params ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 8
            :outdoc '("numatoms" "onset" "duration" "magnitude" "norm" "corpus-index" "file-index" "filepath")
            (let* ((sdiflist (flat (getsdifdata self 0 "XADS" "XSGM" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist))
                   (samplerate 16000)
                   (reci-fs (/ 1 samplerate)))
              (values 
               (length sdiflist)                    ;numatoms             
               (om* reci-fs (first translist))     ;onset (sec)
               (om* reci-fs (second translist))      ;duration (sec)
               (sixth translist)                    ;magnitude (lin)
               (fifth translist)                    ;norm (lin)
               (om-round (third translist))        ;corpus-index (int)
               (om-round (fourth translist))         ;file-index (int)
               (get-sgnct-paths self (om-round (third translist)) (om-round (fourth translist))) ;filepath (string)
              )))

#|
(defmethod! get-sgnct-array ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the sgnct array")
            (let* ((thedata (multiple-value-list (get-sgnct-params self mintime maxtime)))
                   (thearray (make-instance 'sgnct-array 
                                            :numcols (first thedata)
                                            :onset (second thedata)
                                            :duration (third thedata)
                                            :magnitude (fourth thedata)
                                            :norm (fifth thedata)
                                            :corpus-index (sixth thedata)
                                            :file-index (nth 6 thedata)
                                            :filepath (nth 7 thedata)
                                            )))
              thearray))

(defmethod! get-sgnct-array-resamp ((self sdiffile) (numcols integer) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled sgnct array")
            (let* ((thedata (multiple-value-list (get-sgnct-params self mintime maxtime)))
                   (corpus-index (simple-bpf-from-list '(0 1) (sixth thedata) 'bpf 15))
                   (file-index (simple-bpf-from-list '(0 1) (nth 6 thedata) 'bpf 15))
                   (sampled-corpus-index (third (multiple-value-list (om-sample corpus-index numcols))))
                   (sampled-file-index (third (multiple-value-list (om-sample file-index numcols))))
                   (thearray (make-instance 'sgnct-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second thedata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third thedata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth thedata) 'bpf 15)   
                                            :norm (simple-bpf-from-list '(0 1) (fifth thedata) 'bpf 15)
                                            :corpus-index corpus-index
                                            :file-index file-index
                                            :filepath (get-sgnct-paths self (om-round sampled-corpus-index) (om-round sampled-file-index))
                   )))
              thearray))



(defmethod objfromobjs ((self sdiffile) (type sgnct-array))
  (let* ((sdifdata (multiple-value-list (get-sgnct-params self)))
         (new (make-instance 'sgnct-array 
               :numcols (first sdifdata)
               :onset (second sdifdata)
               :duration (third sdifdata)
               :magnitude (fourth sdifdata)
               :norm (fifth sdifdata)
               :corpus-index (sixth sdifdata)
               :file-index (nth 6 sdifdata)
               :filepath (nth 7 sdifdata)
               )))
    new))
|#


(defmethod! get-sgnct-paths ((self sdiffile) (corpusid list) (fileid list))
            (mapcar (lambda (corpora files)
                      (get-sgnct-paths self corpora files)) corpusid fileid)
            )

(defmethod! get-sgnct-paths ((self sdiffile) (corpusid number) (fileid number))
            (let* ((nvtlist (getnvtlist self))
                  (nvtkeys (loop for nvt in nvtlist collect 
                                 (find-in-nvt nvt "TableName")))
                  (corpusnvt (nth (- (stringposition "CorpusDirectories" nvtkeys) 1) nvtlist))
                  (corpuspath (find-in-nvt corpusnvt (integer-to-string corpusid)))
                  (filenvt (nth (- (stringposition (format nil "Corpus-~d" corpusid) nvtkeys) 1) nvtlist))
                  (filename (find-in-nvt filenvt (integer-to-string fileid)))
                  (filepath (pathname (format nil "~a/~a" corpuspath filename))))
              filepath)
            )

#|
(defun sgnct-amplitude (norm magnitude)
  (om/ 1 (om-abs (om* norm (om+ magnitude 0.00000001)))))

; (om/ 1 (om-abs (om* thenorm themagnitude)))   ;amplitude (lin)
|#

(defun sgnct-amplitude (norm magnitude)
  (om* (om/ 1 norm) (om-abs magnitude)))

(defun stringposition (thestring thelist)
  (nth 0 (remove nil 
                 (loop 
                  for index from 1 to (length thelist) 
                  for item in thelist collect
                  (when (string-equal item thestring)
                  index)
                  ))))

; NOTES =================
; 
; I need a function to convert an array back into an SDIF file.