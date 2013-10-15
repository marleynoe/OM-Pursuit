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

;%%%%%%%%%%%%%% FILE I/O FUNCTIONS %%%%%%%%%%%%%%%


(defmethod! in-directory (&key (unix nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a directory pathname .Opens a dialog window to choose the directory. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path (om-choose-directory-dialog)))
                (if unix
                    (namestring path)
                  path)))


(defmethod! in-directories (&key (unix nil) (recursive nil))
              :icon '(250)
              :indoc '("unix format")
              :doc "Returns a directory pathname. Opens a dialog window to choose the directory. If <unix> is T then the output files is formatted for Unix and system commands."
              (let ((path 
                     (if recursive
                         (recurse-dirs (om-choose-directory-dialog))
                       (om-choose-directory-dialog))))
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

(defmethod! in-files (&key (unix nil) (type nil) (directories nil) (files t) (resolve-aliases nil) (hidden-files nil) (path nil))
            :icon '(250)
            :doc "Returns a list of file pathnames. Opens a dialog window to choose the input-directory. If <unix> is T then the output files is formatted for Unix and system commands."
            (let* ((thepath (or path (in-directory :unix unix)))
                  (thefilelist (om-directory thepath 
                                             :type type :directories directories :files files 
                                             :resolve-aliases resolve-aliases :hidden-files hidden-files)))
              thefilelist))

;; HANDLE EXISTING DIRS

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

;;; READ A FILE

(defun read-file (self)
 (with-open-file (f self :direction :input)
   (let ((line (read-line f nil 'eof))
         (rep nil))
     (loop while (not (equal line 'eof)) do
           (multiple-value-bind (name rest)
               (string-until-char 
                (remove-if #'(lambda (c) (or (= 194 c) (= 160 c))) line :key 'char-code)
                ":")
             (when name
               (if rest
                   (pushr (list name (read-from-string rest)) rep)
                 (let ((linedata (data-from-line name)))
                   (pushr (list (apply 'concatenate (cons 'string
                                                          (mapcar #'(lambda (item)
                                                                      (concatenate 'string (string item) " "))
                                                                  (butlast linedata))))
                                (car (last linedata)))
                          rep))))
             (setf line (read-line f nil 'eof)))
           )
     rep)))