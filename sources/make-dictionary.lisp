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

#|
; old version
(defmethod! make-dict ((1asosdif sdiffile) (targetsound t) (corpora list) (kbest t) &key (outpath nil) (windowsize 1024) (fftsize 1024) (hopsize 256))

                        :icon 04
                        :initvals '(nil nil nil 5 nil) 
                        :indoc '("string, path to 1ASO SDIF" "path to target sound file" "list of paths to directories containing sound files" 
                                                             "list of ints, select the k best sound files to be included in dictionary for each time/frequency-region"  
                                                             "string, path to write python dictionary object" 
                                                             "STFT window size" "STFT fft size" "STFT hop size" )

                        (if (probe-file *mdc-path*)
                            (let* ((asopath (namestring (filepathname 1asosdif)))
                                   (targetpath (if (sound-p targetsound)
                                                   (namestring (sound-path targetsound))
                                                   (namestring targetsound)))
                                   (numrectangles (length (sdifstreams 1asosdif)))
                                   (outdir (or (and outpath (pathname-directory outpath)) 
                                               ;(om-make-pathname :directory (pathname-directory targetpath))
                                               *om-outfiles-folder*))   
                                   (dictionary-path (om-make-pathname :directory outdir; (append (pathname-directory targetpath))
                                                                           :name (or (and outpath (pathname-name outpath))
                                                                                     (string+ (pathname-name targetpath) "_sge-dictionary")) 
                                                                           :type "dct"))
                                    
                                    (str (format nil "~s '~a' '~a' \"~a\" ~d ~d ~d ~s ~s" 
                                                 (namestring *mdc-path*)
                                                 asopath 
                                                 targetpath
                                                 (python-directories corpora)
                                                 windowsize
                                                 fftsize
                                                 hopsize
                                                 (python-format (or (and kbest (listp kbest))
                                                                    (repeat-n kbest numrectangles)))
                                                 (namestring dictionary-path)
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
|#

; NEW FUNCTION
; target-sound constraint-sdif soundfiles-sdif outpath &key

; I should probably have two methods for the two different applications

(defmethod! make-dict ((targetsound sound) (constraintsdif sdiffile) (corpussdif sdiffile) (outpath t) &key (kbest 5) (windowsize 1024) (fftsize 1024) (hopsize 256))

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

#|
Ok I'll check the server.

The new makeDictionary app is in the DB, the command line calls should go, for markers:


./makeDictionary target_path file_sdif constraint_sdif outpath

and for polygons:

./makeDictionary target_path file_sdif constraint_sdif outpath --winsize int --fftsize int --hopsize int --kbest [int int int]

The kbest might cause some confusion, as before it needs to have a sequence of integers that is the same number in length as there are polygons (though useless having more shouldn't break it).  With the new arg parsing there are no brackets and commas though.  SO take an example with three polygons and you want the best 10, the call will be:

./makeDictionary target_path file_sdif constraint_sdif outpath --winsize int --fftsize int --hopsize int --kbest 10 10 10 
 

A point here that I forgot about is that the kbest actually works per directory (i.e. per corpus in my thinking), so the number given shouldn't exceed the number of files in that corpus.  The indexing comes from the unique directories in the filelist-sdif.  I'm just realizing that this could be a problem if the elements in each NVT are not ordered... bleh.  They seem to be when I do them but I'm not sure on your end.
|#

#|
;this method should make an 'sdiffile' object from the sdifpath
(defmethod! make-dict ((sdifpath pathname) (target t) (corpora list) (kbest list) (outpath pathname) &key (windowsize 512) (fftsize 512) (hopsize 128))
            (make-dict (filepathname sdifpath) target corpora kbest outpath :windowsize windowsize :fftsize fftsize :hopsize hopsize))
|#

#|
(sdif_path, target_path, list_of_corpora, winsize, fftsize, hopsize, list_of_kbest, outpath)
sdif_path := string, path to 1ASO SDIF,
target_path := string, path to target sound,
list_of_corpora := list of strings, paths to directories containing sound files, i.e. ['str1', 'str2' ...]
 winsize := int, STFT window size
fftsize := int, STFT fftsize, 
hopsize := STFT hop size, 
list_of_kbest := list of ints, select the k best sound files to be included in dictionary for each polygon (k must be smaller than the number of files in the corpus, of course)
outpath := string, path to write python dictionary object
|#
