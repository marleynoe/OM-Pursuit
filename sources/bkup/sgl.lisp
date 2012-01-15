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

; I need a function to convert an array back into an SDIF file.

(defmethod! sgl-params (&key (markers '((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2))) (corpora t)  
                               (max-atoms 5) (min-deviation 0.001))
                        :icon 04
                        :initvals '(((0.1 0.2) (0.1 0.2) (0.1 0.2) (0.1 0.2)) nil 5 0.001)
                        :indoc '("markers (sec)" "pathnames/strings pointing to directories containing soundfiles"  "max number of atoms" "min-deviation")
                        (let* (
                               (thestring (format nil "\"~a\" '~a' ~d ~d" 
                                                  (python-directories corpora) 
                                                  markers
                                                  max-atoms min-deviation)))
                          thestring))

(defmethod! sgl-decomp ((snd sound) (partials sdiffile) (commands string) &key outpath (molecules 0))
            :icon 04
            :numouts 3
            :initvals '(nil nil nil nil 0)
            :outdoc '("decomposition-sdif-file" "decomposition-audio-file" "residual-audio-file")
            :menuins '((4 (("no molecules" 0) ("render molecules" 1))))

            (if (probe-file *sgl-path*)
                (let* ((inpath (sound-path snd))
                       (partialsfile (namestring (filepathname partials)))
                       (filename (pathname-name inpath))
                       (name (or (and outpath (pathname-name outpath)) (format nil "~a_sgl" filename)))
                       (outdir (or outpath (make-pathname :directory (append (pathname-directory inpath) (list name)))))                      
                       (sdif-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                       :name (string+ filename "_soundgrain_decomp") :type "sdif"))
                       (audio-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_sgl-synthesis") :type "wav"))
                       (residual-outfile (om-make-pathname :directory (append (pathname-directory outdir))
                                                        :name (string+ filename "_sgl-residual") :type "wav"))
                       )
                  (om-create-directory outdir)
                  (setf str 
                        (format nil "~s ~s ~s ~s ~a ~d" 
                                (namestring *sgl-path*)
                                (namestring inpath)
                                (namestring partialsfile)
                                (namestring outdir)
                                commands
                                molecules
                                ))
                  ;(print str)
                  ;(print outdir)
                  ;(print sdif-outfile)
                  ;(print audio-outfile)
                  ;(print residual-outfile)
                  (om-cmd-line str *sys-console*)
                  (values (probe-file sdif-outfile) (probe-file audio-outfile) (probe-file residual-outfile))           
                  )
              (progn 
                (print "sglDecomp not found... set in preferences?")
                nil
                ))
            )

(defmethod! get-sgl-params ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 11
            :outdoc '("numatoms" "onset" "duration" "amplitude" "magnitude" "norm" "corpus-index" "file-index" "filepath" "pitch" "velocity" "molecule")
            (let* ((sdiflist (flat (getsdifdata self 0 "XADS" "XSLM" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist))
                   (samplerate (get-decomp-fs self))
                   (reci-fs (/ 1 samplerate)))
              (values 
               (length sdiflist)                    ;numatoms             
               (om* reci-fs (second translist))     ;onset (sec)
               (om* reci-fs (third translist))      ;duration (sec)
               (nth 8 translist)                    ;magnitude (lin)
               (sixth translist)                    ;norm (lin)
               (om-round (fourth translist))        ;corpus-index (int)
               (om-round (fifth translist))         ;file-index (int)
               (get-soundgrain-paths self (fourth translist) (fifth translist)) ;filepath (string)
               (seventh translist)                  ;pitch (mc)
               (nth 7 translist)                    ;velocity (midi)               
               (om-round (first translist)))        ;molecule (int))
              ))

; (om/ 1 (om-abs (om* thenorm themagnitude)))   ;amplitude (lin)

(defmethod! get-sgl-array ((self sdiffile) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the sgl array")
            (let* ((thedata (multiple-value-list (get-sgl-params self mintime maxtime)))
                   (thearray (make-instance 'sgl-array 
                                            :numcols (first thedata)
                                            :onset (second thedata)
                                            :duration (third thedata)
                                            :magnitude (fourth thedata)
                                            :norm (fifth thedata)
                                            :corpus-index (sixth thedata)
                                            :file-index (nth 6 thedata)
                                            :filepath (nth 7 thedata)
                                            :pitch (nth 8 thedata)
                                            :velocity (nth 9 thedata)
                                            :molecule (nth 10 thedata)
                                            )))
              thearray))

(defmethod! get-sgl-array-resamp ((self sdiffile) (numcols integer) &optional mintime maxtime)
            :icon 04
            :numouts 1
            :outdoc '("the re-sampled sgl array")
            (let* ((thedata (multiple-value-list (get-sgl-params self mintime maxtime)))
                   (corpus-index (simple-bpf-from-list '(0 1) (sixth thedata) 'bpf 15))
                   (file-index (simple-bpf-from-list '(0 1) (nth 6 thedata) 'bpf 15))
                   (sampled-corpus-index (third (multiple-value-list (om-sample corpus-index numcols))))
                   (sampled-file-index (third (multiple-value-list (om-sample file-index numcols))))
                   (thearray (make-instance 'sgl-array 
                                            :numcols numcols
                                            :onset (simple-bpf-from-list '(0 1) (second thedata) 'bpf 15)
                                            :duration (simple-bpf-from-list '(0 1) (third thedata) 'bpf 15)
                                            :magnitude (simple-bpf-from-list '(0 1) (fourth thedata) 'bpf 15)   
                                            :norm (simple-bpf-from-list '(0 1) (fifth thedata) 'bpf 15)
                                            :corpus-index corpus-index
                                            :file-index file-index
                                            :filepath (get-soundgrain-paths self sampled-corpus-index sampled-file-index)
                                            :pitch (simple-bpf-from-list '(0 1) (nth 8 thedata) 'bpf 15)
                                            :velocity (simple-bpf-from-list '(0 1) (nth 9 thedata) 'bpf 15)
                                            :molecule (simple-bpf-from-list '(0 1) (nth 10 thedata) 'bpf 0))
                   ))
              thearray))


(defmethod objfromobjs ((self sdiffile) (type sgl-array))
  (let* ((sdifdata (multiple-value-list (get-sgl-params self)))
         (new (make-instance 'sgl-array 
               :numcols (first sdifdata)
               :onset (second sdifdata)
               :duration (third sdifdata)
               :magnitude (fourth sdifdata)
               :norm (fifth sdifdata)
               :corpus-index (sixth sdifdata)
               :file-index (nth 6 sdifdata)
               :filepath (nth 7 sdifdata)
               :pitch (nth 8 sdifdata)
               :velocity (nth 9 sdifdata)
               :molecule (nth 10 sdifdata)
               )))
    new))

(defun sgl-amplitude (norm magnitude)
  (om/ 1 (om-abs (om* norm (om+ magnitude 0.00000001)))))
