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

           
(defclass! gabor-array (parametric-array)
           (
            (molecule     :accessor molecule     :initarg :molecule     :initform nil)
           )
           (:icon 03)
           )

; %%%% OBJFROMOBJS for Chroma classes
#|
(defmethod objfromobjs ((self gabor-array) (type fm-2))
  (let* ((gabordata (data self))
         (frequency (fifth gabordata))
         (new (make-instance 'fm-2
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs (second gabordata)
                             :amp (gabor-amplitude (fourth gabordata) (third gabordata))
                             :freq (om* .5 frequency)
                             :fmod frequency; (om* 0.5 frequency)
                             :imax (om/ (seventh gabordata) frequency)
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(4 3))
                             :ienv (make-cs-table 'Gen20  '(0 4096) '(6 1))             
                             )))
    new))


(defmethod objfromobjs ((self gabor-array) (type fof-a1))
  (let* ((gabordata (data self))
         (frequency (fifth gabordata))
         (duration (second gabordata))
         (halftime (om* 0.25 duration)) ;1/4 seems to be best
         (new (make-instance 'fof-a1
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs duration
                             :amp (om* 10 (gabor-amplitude (fourth gabordata) (third gabordata)))
                             :f0 frequency
                             :freq frequency
                             :bw (seventh gabordata)
                             :aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097)
                             :win halftime
                            ; :wdur duration ;(om- duration 0.001) ;duration doesn't work ... why?
                             :wout halftime
                             :phs (rad->norm (sixth gabordata))
                             )))
    new))


(defmethod objfromobjs ((self gabor-array) (type add-1))
  (let* ((gabordata (data self))
         (new (make-instance 'add-1
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs (om* 20 (second gabordata))
                             :amp (om* 1000 (gabor-amplitude (fourth gabordata) (third gabordata)))
                             :aenv (loop for bw in (om-scale (seventh gabordata) 2.0 0.1) collect ;6 0.5
                                         (make-cs-table 'Gen-20  '(0 1) (list '6 bw)  1 "?" 4097))                         
                             :freq (fifth gabordata)
                             )))
    new))

(defmethod objfromobjs ((self gabor-array) (type add-a1))
  (let* ((gabordata (data self))
         (new (make-instance 'add-a1
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs (om* 100 (second gabordata))
                             :amp (om* 1000 (gabor-amplitude (fourth gabordata) (third gabordata)))
                             :aenv (loop for bw in (om-scale (gabor-bandwidth (second gabordata)) 0.1 2.0) collect; 
                                         (make-cs-table 'Gen-20  '(0 1) (list '6 bw)  1 "?" 4097)) 
                             :freq (fifth gabordata)
                             :fdev (sixth gabordata) ;sixth=phase, seventh=bandwidth ; eigth=molecule
                             )))
    new))

|#

#|

(defmethod objfromobjs ((self gabor-array) (type xavier))
  (let* ((gabordata (data self))
         (frequency (fifth gabordata))
         (themagnitude (third gabordata))
         (thenorm (fourth gabordata))
         (duration  (second gabordata))
         (halftime (om* 0.5 duration)) ;1/4 seems to be best
         (new (set-array 'xavier 
                         (length (car gabordata))
                         (list 
                          'e-dels (first gabordata)
                          'durs duration
                          'amp (gabor-amplitude (fourth gabordata) (third gabordata)) ;(om* 10 
                          'aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097); (make-cs-table 'Gen20  '(0 4096) '(3 1) 1 "?" 4097)
                          'f0 frequency
                          'freq frequency
                          'fdev 0 ;bandwidth / skirtwidth?
                          'bw (nth 6 gabordata)
                          'bwenv (make-cs-table 'Gen20  '(0 4096) '(6 0.1 2) 1 "?" 4097)
                          ;'win halftime
                          ;'wdur duration
                          ;'wout halftime
                       ;   'freqjit-aenv (make-cs-table  'Gen-07  '(0 513) ' (0 0) 1 "?" 513) ;eventually defaults
                          ;'freqjit-fenv (make-cs-table  'Gen-07  '(0 513) '(50 50) 1 "?" 513)
                       ;   'bwjit-aenv (make-cs-table  'Gen-07  '(0  512) '(0 0) 1 "?" 513) ;eventually use defaults
                          ;'bwjit-fenv (make-cs-table  'Gen-07  '(0 513) '(15 15) 1 "?" 513)
                           'phs (rad->norm (sixth gabordata))
                          ; KEYWORDS
                          'magnitude themagnitude
                          'norm thenorm
                          'molecule (nth 7 gabordata)
                          )))) 
    new))


(defmethod objfromobjs ((self gabor-array) (type xavier))
  (let* ((gabordata (data self))
         (frequency (fifth gabordata))
         (duration (second gabordata))
         (halftime (om* 0.25 duration)) ;1/4 seems to be best
         (new (make-instance 'xavier
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs duration
                             :amp (om* 10 (gabor-amplitude (fourth gabordata) (third gabordata)))
                             :f0 frequency
                             :freq frequency
                             :bw (seventh gabordata)
                             :aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097)
                             :win halftime
                            ; :wdur duration ;(om- duration 0.001) ;duration doesn't work ... why?
                             :wout halftime
                             :phs (rad->norm (sixth gabordata))
                             )))
    new))

(defmethod objfromobjs ((self gabor-array) (type struck-string))
  (let* ((gabordata (data self))
         (new (make-instance 'struck-string
                             :numcols (length (first gabordata))
                             :e-dels (first gabordata)
                             :durs (second gabordata)
                             :amp (om-scale (third gabordata) 0.0 1.0)
                             :freq (fourth gabordata)
                             :strikevelocity (om-scale-exp (om/ 1 (second gabordata))  0 10000 0.5) ;this can now be dependent of bandwidth!
                             )))
    new))

(defmethod objfromobjs ((self gabor-array) (type johnny))
  (let* ((gabordata (data self))
         (thefrequency (fifth gabordata))
         (themagnitude (third gabordata))
         (thenorm (fourth gabordata))
         (newarray (set-array 'johnny 
                              (length (car gabordata))
                              (list 'e-dels (first gabordata)
                                    'durs (om* 10 (second gabordata))
                                    'amp (gabor-amplitude thenorm themagnitude)
                                    'aenv (loop for bw in (om-scale (gabor-bandwidth (second gabordata)) .1 2.0) collect
                                               (make-cs-table 'Gen-20  '(0 1) (list '6 bw)  1 "?" 4097)) ; maybe make this a mapping
                                    'carfreq (om* 0.5 thefrequency)
                                    'carfdev 0
                                    'modfreq thefrequency
                                    'index (om-scale (second gabordata) 5 1)
                                    'indexdev (om-scale (gabor-bandwidth (second gabordata)) 1 50)
                                    'indexenv (make-cs-table 'Gen05  '(0 4096) '(1 0.1) 3 "?" 513); (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097) 
                                    'phi (rad->norm (sixth gabordata))
                                    'freqjit-aenv (make-cs-table  'Gen-07  '(0 513) '(0 0) 1 "?" 513)
                                    'indexjit-aenv (make-cs-table  'Gen-07  '(0 256 513) '(0 50 0) 1 "?" 513)
                                    ; KEYWORDS
                                    'magnitude themagnitude
                                    'norm thenorm
                                    'bandwidth (seventh gabordata)
                                    'molecule (nth 7 gabordata)                                    
                                                    ))))
    newarray))


(defmethod objfromobjs ((self gabor-array) (type addison))
  (let* ((gabordata (data self))
         (themagnitude (third gabordata))
         (thenorm (fourth gabordata))
         (newarray (set-array 'addison 
                              (length (car gabordata))
                              (list 'e-dels (first gabordata)
                                    'durs (second gabordata)
                                    'amp (gabor-amplitude thenorm themagnitude)
                                    ;'aenv (loop for bw in (om-scale (gabor-bandwidth (second gabordata)) 2.0 6.0) collect
                                    ;           (make-cs-table 'Gen-20  '(0 1) (list '6 bw)  1 "?" 4097)) ; maybe make this a mapping
                                    'freq (fifth gabordata)
                                    'fdev 0
                                    'phi (rad->norm (sixth gabordata))
                                    'ampjit-aenv (make-cs-table  'Gen-07  '(0 513) '(0 0) 1 "?" 513)
                                    'freqjit-aenv (make-cs-table  'Gen-07  '(0 513) '(0 0) 1 "?" 513)
                                    ; KEYWORDS
                                    'magnitude themagnitude
                                    'norm thenorm
                                    'bandwidth (seventh gabordata)
                                    'molecule (nth 7 gabordata)                                    
                                                    ))))
    newarray))

#|
(defmethod objfromobjs ((self gabor-array) (type addison))
  (let* ((gabordata (data self))
         (newarray (set-array 'addison 
                              (length (car gabordata))
                              (list 'e-dels (first gabordata)
                                    'durs (om* 10 (second gabordata))
                                    'amp (gabor-amplitude (fourth gabordata) (third gabordata))
                                    'aenv (loop for bw in (om-scale (gabor-bandwidth (second gabordata)) 0.1 6.0) collect ;0.1 2.0)
                                               (make-cs-table 'Gen-20  '(0 1) (list '6 bw)  1 "?" 4097))
                                    'freq (fifth gabordata)
                                    ;'fdev 0
                                    'phi (rad->norm (sixth gabordata))
                                                    ))))
    newarray))
|#

|#