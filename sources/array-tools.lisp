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

;%%%%%%%%%% PROCESS ARRAY %%%%%%%%%%%%%%

;;make methods for lists of processes!

(defmethod! build-array ((components list))
            :icon '(264)
            (let* ((newarray (make-instance (type-of (comp-array (first components)))
                                            :numcols 0
                                            ))) ;(or actiontime 0)
              (mapcar (lambda (thecomps)
                        (add-comp newarray thecomps)) components)
              newarray
              ))

(defmethod! merge-arrays ((self class-array) &rest arrays)
            :icon '(264)
            (let* ((arraylist (flat (x-append self arrays)))
                   (complist 
                    (loop for array in arraylist collect
                          (get-components array)))
                    ;(mapcar (lambda (theindex)
                    ;          (get-comp array theindex)) (arithm-ser 0 (1- (numcols array)) 1))))
                   (thenewarray (build-array (flat complist))))
              thenewarray))

(defmethod! get-components ((array class-array))
            :icon '(323)
            (mapcar (lambda (theindex)
                      (get-comp array theindex)) (arithm-ser 0 (1- (numcols array)) 1)))

(defmethod! get-components ((array list))
  (flat (mapcar (lambda (thearrays)
            (get-components thearrays)) array)))

(defmethod! add-slot ((self class-array) (slotname string) (slotvals t))
            :icon '(264)
            (let* (;(thearray (clone self))
                   (arraydata (data self))
                   (labeldata
                    (loop for slot in arraydata
                          for i from 0 to (1- (length arraydata))
                          collect
                          (list (index2label self i) slot)
                          ))
                   (finaldata (x-append (flat labeldata 1) slotname (list slotvals))))
              (set-array (type-of self) (numcols self) finaldata)
              ))

#|
(defmethod! set-array-slot ((self class-array) (slotname t) (slotvals t))
            :icon '(264)
            (let* ((arraydata (data self))
                   (slotname (list! slotname))
                   (slotvals (if (equal 1 (length slotname))
                                 (list slotvals)
                               (list! slotvals)))
                   (labeldata
                    (loop for slot in slotname
                          for vallist in slotvals
                          collect
                          (list slot vallist)
                          )))
              ;(print (flat labeldata 1))
              (set-array (type-of self) (numcols self) (flat labeldata 1))
              ))
|#

(defmethod! set-array-slot ((array class-array) (slotname string) (slotvals t))
            :icon '(264)
            (let ((newarray array))
           ; (setf ((label2index newarray slotname) newarray) slotvals)
            ;  (label2index newarray slotname)
              ;(setf (#'slotname newarray) slotvals)
              (setf #'(lambda (theslotname)
                      (theslotname newarray) slotname) slotvals)
            (set-data newarray)
            newarray         
            ))



(defun set-array (type numcols params)
(let ((array (cons-array (make-instance type) (list nil numcols 0 nil) params)))
 (set-data array)
 array)
)


(defmethod! process-array ((process t) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  (complist (loop for i from 0 to (1- (numcols thearray))
                                   collect (get-comp thearray i))))
                    (funcall process (list thearray complist))
            thearray
            ))

(defmethod! process-array-field ((process t) (array class-array))
            :icon '(264)
            (let ((thearray (clone array)))
                    (apply process (list thearray)) ;what's the difference to 'funcall' ?
            thearray
            ))

(defmethod! process-array-comp ((process t) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                   (complist (loop for i from 0 to (1- (numcols thearray))
                                   collect (get-comp thearray i))))
              (loop for comp in complist do
                    (apply process (list comp)))
              thearray
              ))

;;this doesn't work yet... not so easy with lists of processes
(defmethod! process-array-comp ((process list) (array class-array))
            (mapcar (lambda (theprocess)
                      (process-array-comp theprocess array)) process)
            )

(defmethod! process-array-slot ((process t) (slotname string) (array class-array))
            :icon '(264)
            (let* ((thearray (clone array))
                  ;(theslotvalues (symbol-function slotname)) ;later I should use symbol-function... but no time for now
                  (theslotvalues (array-field array slotname))
                  (thenewvalues (funcall process theslotvalues)))
            (array-field thearray slotname thenewvalues)
            thearray
            ))



;%%%%%%%%%%%%%% USER FUNS %%%%%%%%%%%%%%%%%%%%%%

; a number of pre-determined processing funs for arrays

;; component methods %%%%%%%%%%%%%%


(defmethod! ran-env ((self component) (slotname string) (min number) (max number))
            :icon 04
            :initvals '(nil nil nil nil)
            (comp-field self slotname (gen-window (om-random min max)))
            )

(defmethod! comp-bandfilter ((self component) (slotname string) (minval number) (maxval number))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue minval) (> thevalue maxval))
                (remove-comp self)
              thevalue)
            ))

(defmethod! comp-bandfilter ((self component) (slotname string) (minval list) (maxval list))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue (nth  (index self) minval)) 
                    (> thevalue (nth  (index self) maxval)))
                (remove-comp self)
              thevalue)
            ))

(defmethod! comp-bandfilter ((self component) (slotname string) (minval string) (maxval string))
            :icon 04
            (let ((thevalue (comp-field self slotname)))
            (if (or (<= thevalue (comp-field self minval)) 
                    (> thevalue (comp-field self maxval)))
                (remove-comp self)
              thevalue)
            ))

(defmethod! field-quantize ((self component) (slotname string) (interval number))
            :icon 04
            (comp-field self slotname (quantize (comp-field self slotname) interval))
            )

(defmethod! comp-quantize ((self component) (interval list))
            :icon 04
            (comp-list self (quantize (comp-list self) interval))
            )

(defmethod! field-perturbation ((self component) (slotname string) (amount number))
            :icon 04
            (comp-field self slotname (perturbation (comp-field self slotname) (* 0.01 amount)))
            )

(defmethod! comp-perturbation ((self component) (amount number))
            :icon 04
            (comp-list self (perturbation (comp-list self) (* 0.01 amount)))
            )

(defmethod! comp-perturbation ((self component) (amount list))
            :icon 04
            (comp-list self (perturbation (comp-list self) (om* 0.01 amount)))
            )

;catch exceptions
(defmethod! perturbation ((self t) number)
            self)
  


;;; array-field methods %%%%%%%%%%%%%%%%%%%%%%%


(defmethod! field-reduce ((self class-array) (slotname string) (points integer) &key slotname-x (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (xvals (if slotname-x
                              (array-field self slotname-x)
                            (arithm-ser 0 (1- numcomps) 1)))
                   (yvals (array-field self slotname))
                   (curve (mat-trans (list xvals yvals))))
              (array-field self slotname (third (multiple-value-list (om-sample (second (mat-trans (reduce-n-points curve points precision))) numcomps))))
              ))

(defmethod! field-reduce ((self class-array) (slotname string) (points float) &key slotname-x (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length (car (data self))))
                   (xvals (if slotname-x
                              (array-field self slotname-x)
                            (arithm-ser 0 (1- numcomps) 1)))
                   (yvals (array-field self slotname))
                   (curve (mat-trans (list xvals yvals))))
              (array-field self slotname (third (multiple-value-list (om-sample (second (mat-trans (reduce-points curve (* 0.01 (- 100 points))))) numcomps))))
              ))

(defmethod! field-lowpass ((self class-array) (slotname string) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nilarray-data nil "lowpass" 3 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname (filtres::low-pass-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "median")
                     (array-field self slotname (filtres::median-filter-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "mean")
                     (array-field self slotname (filtres::mean-filter-rec thedata windowsize recursion-depth)))
                    ))
              )

;maybe also for the high-pass and bandpass I should introduce scales (for rescaling the ranges of the data...

(defmethod! field-highpass ((self class-array) (slotname string) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil nil "lowpass" 3 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname (om- thedata (filtres::low-pass-rec thedata windowsize recursion-depth))))
                    ((equal filtertype "median")
                     (array-field self slotname (om- thedata (filtres::median-filter-rec thedata windowsize recursion-depth))))
                    ((equal filtertype "mean")
                     (array-field self slotname (om- thedata (filtres::mean-filter-rec thedata windowsize recursion-depth))))
                    ))
              )

(defmethod! field-bandpass ((self class-array) (slotname string) (filtertype string) (windowsize-h number) (recursion-depth-h number) (windowsize-l number) (recursion-depth-l number))
            :icon 04
            :initvals '(nil nil "lowpass" 3 1 2 1)
            :menuins '((2 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))
            (let* ((thedata (array-field self slotname)))           
              (cond ((equal filtertype "lowpass")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::low-pass-rec thedata windowsize-h recursion-depth-h)) 
                                       (om- thedata (filtres::low-pass-rec thedata windowsize-l recursion-depth-l)))))                                      
                    ((equal filtertype "median")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::median-filter-rec thedata windowsize-h recursion-depth-h))
                                       (om- thedata (filtres::median-filter-rec thedata windowsize-l recursion-depth-l)))))                
                    ((equal filtertype "mean")
                     (array-field self slotname 
                                  (om- (om- thedata (filtres::mean-filter-rec thedata windowsize-h recursion-depth-h)) 
                                       (om- thedata (filtres::mean-filter-rec thedata windowsize-l recursion-depth-l)))))
                    ))
              )

(defmethod! field-scale ((self class-array) (slotname string) &key minval maxval exp)
            :icon 04
            :initvals '(nil nil nil nil)
            (let ((thedata (array-field self slotname)))           
            (unless minval
              (setf minval (list-min thedata)))
            (unless maxval
              (setf maxval (list-max thedata)))
            (unless exp
              (setf exp 1.0))
            (array-field self slotname (om-scale-exp thedata minval maxval exp))
            ))

(defmethod! field-sort ((self class-array) (slotname string) (test symbol) &key key)
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thedata (data self))
                   (trans-data (mat-trans thedata)) 
                   (thesorteddata (sort-list trans-data
                                             :test test 
                                             :key #'(lambda (trans-data) (nth (label2index self slotname) trans-data))))
                   (slot-data (mat-trans thesorteddata)))
              (loop for slot in slot-data
                    for i from 0 to (1- (length slot-data))
                    do
                   (array-field self (index2label self i) slot))
              self
              ))

(defmethod! array-rep-filter ((self list) (slotname string))
            :icon 04
            :initvals '(nil nil nil nil)
            (let* ((thearrayvals (multiple-value-list (array-vals self)))
                   (thearray (first thearrayvals))
                   (thecomplist (second thearrayvals))
                   (thevallist (array-field thearray slotname))
                   (replist (rep-p thevallist)))
              (loop for rep in replist
                    for comp in thecomplist
                    do
                    (if rep
                        (remove-comp comp)
                      comp)
                    ))
            )

; %%%% slot-methods %%%%%%%%

(defmethod! slot-paths ((thedata list) (discard-levels number) (new-dir t) (keep-levels number))
            :icon 04
            (convert-paths thedata discard-levels new-dir keep-levels)
            )


(defmethod! slot-lowpass ((thedata list) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil "lowpass" 3 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))         
              (cond ((equal filtertype "lowpass")
                     (filtres::low-pass-rec thedata windowsize recursion-depth))
                    ((equal filtertype "median")
                     (filtres::median-filter-rec thedata windowsize recursion-depth))
                    ((equal filtertype "mean")
                     (filtres::mean-filter-rec thedata windowsize recursion-depth))
                    ))

(defmethod! slot-highpass ((thedata list) (filtertype string) (windowsize number) (recursion-depth number))
            :icon 04
            :initvals '(nil "lowpass" 3 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))         
              (cond ((equal filtertype "lowpass")
                     (om- thedata (filtres::low-pass-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "median")
                     (om- thedata (filtres::median-filter-rec thedata windowsize recursion-depth)))
                    ((equal filtertype "mean")
                     (om- thedata (filtres::mean-filter-rec thedata windowsize recursion-depth)))
                    ))

(defmethod! slot-bandpass ((thedata list) (filtertype string) (windowsize-h number) (recursion-depth-h number) (windowsize-l number) (recursion-depth-l number))
            :icon 04
            :initvals '(nil "lowpass" 3 1 2 1)
            :menuins '((1 (("lowpass" "lowpass") ("median" "median") ("mean" "mean"))))         
            (cond ((equal filtertype "lowpass")
                   (om- (om- thedata (filtres::low-pass-rec thedata windowsize-h recursion-depth-h)) 
                        (om- thedata (filtres::low-pass-rec thedata windowsize-l recursion-depth-l))))    
                  ((equal filtertype "median")
                   (om- (om- thedata (filtres::median-filter-rec thedata windowsize-h recursion-depth-h))
                        (om- thedata (filtres::median-filter-rec thedata windowsize-l recursion-depth-l))))
                  ((equal filtertype "mean")
                   (om- (om- thedata (filtres::mean-filter-rec thedata windowsize-h recursion-depth-h)) 
                        (om- thedata (filtres::mean-filter-rec thedata windowsize-l recursion-depth-l))))
                  ))
     
                                

(defmethod! slot-scale ((thedata list) &key minval maxval exp)
            :icon 04
            :initvals '(nil nil nil nil)
            (unless minval
              (setf minval (list-min thedata)))
            (unless maxval
              (setf maxval (list-max thedata)))
            (unless exp
              (setf exp 1.0))
            (om-scale-exp thedata minval maxval exp)
            )

(defmethod! slot-reduce ((thedata list) (points integer) &key (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length thedata))
                   (xvals (arithm-ser 0 (1- numcomps) 1))
                   (yvals thedata)
                   (curve (mat-trans (list xvals yvals))))
              (third (multiple-value-list (om-sample (second (mat-trans (reduce-n-points curve points precision))) numcomps)))
              ))

(defmethod! slot-reduce ((thedata list) (points float) &key (precision 10))
            :icon 04
            :initvals '(nil nil nil nil 10)
            (let* ((numcomps (length thedata))
                   (xvals (arithm-ser 0 (1- numcomps) 1))                            
                   (yvals thedata)
                   (curve (mat-trans (list xvals yvals))))
              (third (multiple-value-list (om-sample (second (mat-trans (reduce-points curve points))) numcomps)))
              ))



;;; HELPER FUNCTIONS ______________________

; I can use 'index' on the component here to save some ressources

(defmethod! array-field ((self class-array) (slotname string) &optional newvalues)
            :icon '(323)
            :initvals '(nil nil nil)
            :outdoc '("the slotvalues")
            (if  newvalues 
                (let ((newvalues (list! newvalues))
                      (indices (arithm-ser 0 (length newvalues) 1)))
                  (print newvalues)
                  (mapcar (lambda (thevalue theindex)
                            (comp-field (get-comp self theindex) slotname thevalue)) 
                          newvalues indices))
                (nth (label2index self slotname) (data self)))
            )

(defmethod! array-field ((self class-array) (slotname list) &optional newvalues)
            (flat (mapcar (lambda (theslotname thenewvalues)
                      (array-field self theslotname thenewvalues)) slotname newvalues)
            ))


(defmethod! get-comp-vals ((self component) (thefunction t) &rest slotnames)
            :icon '(323)
            (funcall thefunction 
                     (list self 
                           (mapcar (lambda (theslotname)
                                     (comp-field self theslotname)) 
                                   slotnames)
                           slotnames)
                     ))

(defmethod! comp-vals ((self list))
            :icon '(323)
            :numouts 3
            :outdoc '("compoonent" "list of lists of slot values" "list of lists of slotnames")
  (values (first self) (second self) (third self))
  )

(defmethod! array-vals ((self list))
            :icon '(323)
            :initvals '(nil nil)
            :numouts 2
            :outdoc '("array" "list of components")
            (values (first self) (second self))
            )


#|
;with three args
(defmethod! get-comp-vals ((self component) (thefunction t) &rest slotnames)
            (funcall thefunction 
                     self 
                     (mapcar (lambda (theslotname)
                               (comp-field self theslotname)) 
                             slotnames)
                     slotnames)
                     )
|#

(defmethod! quantize ((self t) interval)
            (if (and self interval)
                (om* (om-round (om/ self interval) ) interval)
              self
              ))

(defmethod! quantize ((self list) (interval list))
            (mapcar (lambda (thecomplist theintervals)
                      (quantize thecomplist theintervals))
                    self interval))

;; this would be cool with a 'depth' parameter... that is it will only return t after a number of repetitions...

(defun rep-p (thelist)
  (loop for item in thelist
        for otheritem in (x-append 'dummy (butlast thelist))
        collect
        (equal item otheritem)
        ))


