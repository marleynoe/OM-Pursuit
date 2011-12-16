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
                              ;(om-cmd-line str *sys-console*)
                              (sys:run-shell-command str :wait nil)
                              dictionary-path   
                              )
                          (progn
                            (om-beep-msg "executable not found... path set in preferences?")
                            nil))
                        )


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
