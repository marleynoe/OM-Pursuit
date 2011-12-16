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
;Authors: M. Schumacher, G.Boyes

(in-package :om)




;%%%%%%%%%%%%%% PLOTTING FUNCTIONS %%%%%%%%%%%%%%%

(defmethod! partials->bpflib ((self sdiffile) &key mintime maxtime) ;(frametype "1TRC"))
            :icon '(141)
            :numouts 1
            (multiple-value-bind (framelist timelist) (getsdifdata self 0 "1TRC" "1TRC" nil nil nil mintime maxtime)
            (setf partial-list (loop for frame in framelist 
                                     for time in timelist collect    
                                     (loop for partial in frame collect
                                           (x-append partial time))
                                     )))
            (setf grouped-partials (group-indices (flat partial-list 1)))
            (loop for partial in grouped-partials collect
                  (let ((thepartial (mat-trans (cdr partial))))
                  (simple-bpf-from-list (fourth thepartial) (first thepartial) 'bpf 10)
                  )))

#|
(defmethod! file->path ((self t))
            (let* ((thepath (string+ (namestring self) "/")))             
            (make-pathname :directory (pathname-directory thepath))
            ))
|#

#|
;this one is not working yet...
(defmethod! plot-sdifx ((self sdiffile) &optional mintime maxtime)
            :icon 01
            (let* ((sdiflist (flat (getsdifdata self 0 "1ADT" "1ADT" nil nil nil mintime maxtime) 1))
                   (translist (mat-trans sdiflist)))
              (setf thearray
                    (make-instance 'class-array
                                   :numcols (length sdiflist)
                                   :k0 :k0 (first translist)
                                   :k1 (second translist)
                                   :k3 (third translist)
                                   :k4 (fourth translist)
                                   ))
              thearray
              ))
|#

#|
(defclass partials-viewer (editorview) 
  ((streampanels :initform nil :accessor streampanels))
  (:default-initargs))

(defmethod class-has-editor-p ((self gesture-array)) t)
(defmethod get-editor-class ((self gesture-array)) 'gesture-editor)
|#

;should give the bpfs different colors depending on the amplitude

(defmethod! get-bpf-points ((self bpf-lib))
            (let ((bpflist (bpf-list self)))
                    (loop for bpf in bpflist collect
                    (point-pairs bpf)
                    ))
              )


; ---------------------------------------------------------------

(defmethod! group-indices ((self list))
 (let ((reslist nil))
   (loop for item in self do
         (let ((position (position (car item) reslist :test '= :key 'car)))
           (if position
               (pushr (cdr item) (nth position reslist))
             (pushr (list (car item) (cdr item)) reslist))))
   (mapcar 'identity reslist)))

(defmethod! markers->frames ((self list) &key samplerate)
            :icon '(141)
            (when (listp (first self))
              (setf out-array (adt-deep-format (om-round (sec->samples self samplerate)))))
            (when (numberp (first self))
              (setf out-array (adt-format (om-round (sec->samples self samplerate)))))
            (when (sound-p (first self))
              (setf out-array (adt-deep-format (om-round
                               (loop for snd in self collect
                                     (sec->samples (markers snd) (om-sound-sample-rate snd)))))))
              out-array
              )

(defmethod! markers->frames ((self sound) &key samplerate)
                   (let ((out-array (adt-format (om-round (sec->samples (markers self) (om-sound-sample-rate self))))))
            out-array
            ))

(defmethod sound-p ((self sound)) t)
(defmethod sound-p ((self t)) nil) 

;;; for formatting of lists for the python-executable

(defun python-deep-format (input-list)
  (setf thestring (format nil "[~a" (python-format (car input-list))))
  (loop for item in (cdr input-list) collect
         (setf thestring (concatenate 'string thestring "," (python-format item))))
  (setf thestring (concatenate 'string thestring (format nil "]")))
  thestring)


(defun python-format (input-list)
  (setf thestring (format nil "[~d" (first input-list)))
        (loop for item in (cdr input-list) do
              (setf thestring (concatenate 'string thestring
                                           (format nil ",~d" item))))
        (setf thestring (concatenate 'string thestring (format nil "]")))
        thestring)

;%%% for compatibility -2bDeleted-

(defun adt-format (input-list)
  (python-format input-list))

(defun adt-deep-format (input-list)
  (python-deep-format input-list))


(defun om-float (input)
  (loop for item in input collect
        (float item)
        ))

;--------------
;=====getting files+dirs in and out of OM

(defmethod! in-directory (&key (unix nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a directory pathname .Opens a dialog window to choose the directory. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path (om-choose-directory-dialog)))
                (if unix
                    (namestring path)
                  path)))

(defmethod! out-directory (&key (unix nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a directory pathname. Opens a dialog window to enter a directory. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path (om-choose-new-directory-dialog)))
                (setf thepath 
                      (if unix
                          (namestring path)
                        path))
                ;(om-create-directory thepath)
                thepath))

(defmethod! in-file (&key (unix nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a file pathname .Opens a dialog window to choose a file. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path (om-choose-file-dialog)))
                (if unix
                    (namestring path)
                  path)))

(defmethod! out-file (&key (unix nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a file pathname. Opens a dialog window to specify the directory. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path (om-choose-new-file-dialog)))
                (if unix
                    (namestring path)
                  path)))

(defmethod! in-files (&key (unix nil) (type nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil))
            :icon '(250)
            :doc "Returns a list of file pathnames. Opens a dialog window to choose the input-directory. If <unix> is T then the output files is formatted for Unix and system commands."
            (let* ((thepath (in-directory :unix unix))
                  (thefilelist (om-directory thepath 
                                             :type type :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files)))
              thefilelist))

;; handle existing dirs

(defun handle-new-dir-exists (newpath)
  (when (and newpath (probe-file newpath))
    (if *automatic-rename*
        (setf newpath (unique-dir (make-pathname :directory (pathname-directory newpath)
                                                      :host (pathname-host newpath) :device (pathname-device newpath))
                                        ))
      (delete-file newpath)
      ))
  newpath)

;;; FINDS A GOOD (UNIQUE) DIR
(defun unique-dir (dir)
  (let* ((pathname dir)
         (pathdir (pathname-directory pathname)))     
    (loop while (probe-file pathname)
          for i = 1 then (+ i 1) do
          (setf pathname (make-pathname :directory (append (butlast pathdir) 
                                                           (list (format nil "~a~D" (car (reverse pathdir)) i)))
                                        :host (pathname-host pathname) 
                                        :device (pathname-device pathname))))
    pathname))

;;; For now we need to re-define samples->sec function

;;; SAMPLES / SECONDS
(defmethod! samples->sec ((samples number) &optional samplerate)
          :icon 141 
          :initvals '(0 nil)
          :indoc '("number of samples" "sample rate (Hz)")
          :numouts 1
          :doc "Converts <samples> to a time (or duration) in seconds depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the time."
          (float (/ samples (or samplerate *audio-sr*)))
          )

(defmethod! samples->sec ((samples list) &optional samplerate)
   (mapcar #'(lambda (input)
               (samples->sec input samplerate)) samples))

(defmethod! sec->samples ((secs number) &optional samplerate) 
          :icon 141  
          :initvals '(0 nil)
          :indoc '("duration (s)" "sample rate (Hz)")
          :numouts 1
          :doc "Converts <secs> to a number of samples depending on <samplerate>.

If <samplerate> is NIL, the OM default sample rate is used to calculate the samples."
          (float (* secs (or samplerate *audio-sr*))))

(defmethod! sec->samples ((secs list) &optional samplerate) 
          (mapcar #'(lambda (input)
                      (sec->samples input samplerate)) secs)
          )