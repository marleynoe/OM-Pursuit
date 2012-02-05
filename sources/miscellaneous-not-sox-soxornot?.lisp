(in-package :om)

; this helper function removes aif and wav files from a given directory

(defun clean-soundfiles (&optional dir)
  (let ((src-root (or dir (make-pathname :directory (butlast (pathname-directory *load-pathname*) 2)))))
    (mapc #'(lambda (file) 
              (if (system::directory-pathname-p file)
                  (clean-files file)
                (when (and (pathname-type file)
                           (or (string-equal (pathname-type file) "aif")
                               (string-equal (pathname-type file) "wav")))
                  (print (concatenate 'string "Deleting " (namestring file) " ..."))
                  (delete-file file)
                  )))
          (directory (namestring src-root) :directories t))
    ))

; ============= GETTING SDIF MARKERS ===========================
; get any kind of SDIF marker or onsets in voices or chord-seqs


(defmethod! sox-get-markers ((self sdiffile) &key specdistance transient hand quantize mintime maxtime)
            :initvals '(nil t t t nil nil nil)
            ::icon '(608)
            (let ((markers nil)
                  (frametimes (getsdiftimes self 0 "1MRK" nil mintime maxtime)))
                   (when specdistance (setf markers (x-append markers (getsdiftimes self 0 "1MRK" "XASD" mintime maxtime))))
                   (when transient (setf markers (x-append markers (getsdiftimes self 0 "1MRK" "1BEG" mintime maxtime))))
                   (when hand (setf markers (x-append markers 
                                                      (x-diff frametimes (x-append 
                                                                          (getsdiftimes self 0 "1MRK" "XASD" mintime maxtime) 
                                                                          (getsdiftimes self 0 "1MRK" "1BEG" mintime maxtime) 
                                                                          (getsdiftimes self 0 "1MRK" "1END" mintime maxtime))))))
                   (when quantize
                       (setf markers (quantize markers quantize)))
                   (sort-list markers)
              ))

(defmethod! sox-get-markers ((self chord-seq) &key specdistance transient hand quantize mintime maxtime)
            (let* ((markers (om* 0.001 (lonset self))))
                   (when quantize
                       (setf markers (quantize markers quantize)))
                   (sort-list markers)
              ))

(defmethod! sox-get-markers ((self voice) &key specdistance transient hand quantize mintime maxtime)
            (sox-get-markers (ObjfromObjs self (mki 'chord-seq)) :quantize quantize :mintime mintime :maxtime maxtime)
              )

(defmethod! sox-get-markers ((self multi-seq) &key specdistance transient hand quantize mintime maxtime)
            (mapcar (lambda (thechordseqs)
                      (sox-get-markers thechordseqs)) (chord-seqs self)))

(defmethod! sox-get-markers ((self poly) &key specdistance transient hand quantize mintime maxtime)
            (mapcar (lambda (thevoices)
                      (sox-get-markers thevoices)) (voices self)))

(defmethod! sox-get-markers ((self list) &key specdistance transient hand quantize mintime maxtime)
            (mapcar (lambda (thelist)
                      (sox-get-markers thelist 
                                   :specdistance specdistance :transient transient :hand hand 
                                   :quantize quantize :mintime mintime :maxtime maxtime)) self))


; =================================================

(defmethod! transratio ((midicent number))
            :icon '(141)  
            :initvals '(0)
            :indoc '("converts transposition in midicents into transposition-factor")
            :numouts 1
            :doc "bla bla"
            (exp (* midicent 0.00057762265)
                 ))    

(defmethod! transratio ((midicent list))
   (mapcar 'transratio midicent))