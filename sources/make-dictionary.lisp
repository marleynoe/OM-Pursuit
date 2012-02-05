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

; it should later be possible to simply provide a list of paths as the corpussdif and a function will be called internally to convert the list into an sdiffile. Alternatively, the user can have pre-processing steps, e.g. using descriptors etc. to create a subset of these 
; for the future: when having the sound descriptors inside the python function.. and simultaneously having the constraint specifications as an SDIF... I can imagine it opens up many possibilities, for example there's no need of 'labling' atoms for pruning based on for example fundamental frequency as we can get it from each soundfile. then the filtering could take place inside the makeDict function... but it requires having a different collection of atoms for each of the temporal regions.

(defmethod! make-dictionary ((targetsound sound) (constraintsdif sdiffile) (corpussdif sdiffile) (outpath t) &key (kbest 5) (windowsize 1024) (fftsize 1024) (hopsize 256))

                        :icon 04
                        :initvals '(nil nil nil nil 5 1024 1024 256) 
                        :indoc '("path to target sound file"
                                 "string, path to constraints SDIF"  
                                 "string, path to corpus SDIF"
                                 "string, outpath to write python dictionary object"
                                 "list of ints, select the k best sound files to be included in dictionary for each time/frequency-region"  
                                 "STFT window size"
                                 "STFT fft size"
                                 "STFT hop size" )

                        (if (probe-file *mdc-path*)
                            (let* ((targetpath (if (sound-p targetsound)
                                                   (namestring (sound-path targetsound))
                                                   (namestring targetsound)))
                                   (contraintspath (namestring (filepathname constraintsdif)))
                                   (corpuspath (namestring (filepathname corpussdif)))
                                   
                                   (numrectangles (length (sdifstreams constraintsdif)))
                                   (nummarkers (length (get-sdif-markers constraintsdif)))
                                   (outdir (or (and outpath (pathname-directory outpath)) 
                                               ;(om-make-pathname :directory (pathname-directory targetpath))
                                               *om-outfiles-folder*))   
                                   (dictionary-path (om-make-pathname :directory outdir; (append (pathname-directory targetpath))
                                                                           :name (or (and outpath (pathname-name outpath))
                                                                                     (string+ (pathname-name targetpath) "_sge-dictionary")) 
                                                                           :type "dct"))

                                    (str (if (equal (caar (framesdesc constraintsdif)) "1ASO") 
                                             (format nil "~s '~a' '~a' '~a' ~s --winsize ~d --fftsize ~d --hopsize ~d --kbest ~d" 
                                                     (namestring *mdc-path*)
                                                     targetpath                                   
                                                     corpuspath
                                                     contraintspath
                                                     (namestring dictionary-path)
                                                     windowsize
                                                     fftsize
                                                     hopsize
                                                     (python-format-nc (or (and kbest (listp kbest))
                                                                        (repeat-n kbest numrectangles)))
                                                     )
                                           (format nil "~s '~a' '~a' '~a' ~s" 
                                                     (namestring *mdc-path*)
                                                     targetpath 
                                                     corpuspath
                                                     contraintspath
                                                     (namestring dictionary-path)
                                                     )
                                           ))
                                    )
                              (print str)
                              ;(om-cmd-line str *sys-console*)
                              (sys:run-shell-command str :wait nil)
                              dictionary-path   
                              )
                          (progn
                            (om-beep-msg "executable not found... path set in preferences?")
                            nil))
                        )

