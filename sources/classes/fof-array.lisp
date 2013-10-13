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

;maybe later change the order of params into onset, dur, mag, freq, etc. ... (more similar to omchroma)
           

(defclass! fof-array (parametric-array)
           (
            (risetime      :accessor risetime      :initarg :risetime      :initform nil)
            (decaytime     :accessor decaytime     :initarg :decaytime     :initform nil)
            (skirt-width   :accessor skirt-width   :initarg :skirt-width   :initform nil)
            (molecule      :accessor molecule      :initarg :molecule      :initform nil)
            )
           (:icon 03)
           )


; %%%% OBJFROMOBJS for Chroma classes
#|
(defmethod objfromobjs ((self fof-array) (type fm-2))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (new (make-instance 'fm-2
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs (second fofdata)
                             :amp (om* 1000 (fof-amplitude (fourth fofdata) (third fofdata)))
                             :f0 '(0)
                             :freq frequency
                             :fmod frequency
                             :imax (om* 0.01 (nth 9 fofdata))       ;skirt width 
                             :imin (om/ (seventh fofdata) frequency) ;bandwidth
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)
                             :ienv (make-cs-table 'Gen07  '(0 64 1024 4096) '(0 1000 200 0))
                             )))
    new))


; leave these ones here...

(defmethod objfromobjs ((self fof-array) (type fof-a1))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (duration (second fofdata))
         (new (make-instance 'fof-a1
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs duration
                             :amp (om* 1000 (fof-amplitude (fourth fofdata) (third fofdata)))
                             :f0 frequency
                             :freq frequency
                             :bw (seventh fofdata)
                             :aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097)                                       
                             :win (nth 7 fofdata)
                             :wdur duration
                             :wout (nth 8 fofdata)
                             :oct (om* 0.01 (nth 10 fofdata))
                            ; :phs (rad->norm (sixth fofdata)) ;phase doesn't work properly
                             )))
    new))

(defmethod objfromobjs ((self fof-array) (type fof-3))
  (let* ((fofdata (data self))
         (new (make-instance 'fof-3
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs (second fofdata)
                             :amp (om* 1000 (fof-amplitude (fourth fofdata) (third fofdata)))
                             :f0 (fifth fofdata)
                             :freq (fifth fofdata)
                             :bw (seventh fofdata)
                             :aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097)
                             :win (nth 7 fofdata)
                             :wdur (second fofdata) 
                             :wout (nth 8 fofdata)
                             :fdev (sixth fofdata) ;I think I used the inverse of the skirt-width for fdec
                             )))
    new))



(defmethod objfromobjs ((self fof-array) (type fm-2))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (new (make-instance 'fm-2
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs (second fofdata)
                             :amp (om* 1000 (fof-amplitude (fourth fofdata) (third fofdata)))
                             :f0 '(0)
                             :freq frequency
                             :fmod frequency
                             :imax (om* 0.01 (nth 9 fofdata))       ;skirt width 
                             :imin (om/ (seventh fofdata) frequency) ;bandwidth
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)
                             :ienv (make-cs-table 'Gen07  '(0 64 1024 4096) '(0 1000 200 0))
                             )))
    new))

|#
; new Prisma-synthesizers

#|

(defmethod objfromobjs ((self fof-array) (type fof-a1))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (duration (second fofdata))
         (new (make-instance 'fof-a1
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs duration
                             :amp (om* 1000 (fof-amplitude (fourth fofdata) (third fofdata)))
                             :f0 frequency
                             :freq frequency
                             :bw (seventh fofdata)
                             :aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097)                                       
                             :win (nth 7 fofdata)
                             :wdur duration
                             :wout (nth 8 fofdata)
                             :oct (om* 0.01 (nth 10 fofdata)) ; a bit arv
                            ; :phs (rad->norm (sixth fofdata)) ;phase doesn't work properly
                             )))
    new))


(defmethod objfromobjs ((self fof-array) (type xavier))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (themagnitude (third fofdata))
         (thenorm (fourth fofdata))
         (duration (second fofdata))
         (new (set-array 'xavier 
                         (length (car fofdata))
                         (list 
                          'e-dels (first fofdata)
                          'durs duration  ;evtl. longer durs (* 10)
                          'amp (om-scale (fof-amplitude thenorm themagnitude) 0.1 1) ;don't know if this works... with the scaling
                          'aenv (make-cs-table 'Gen07  '(0 4096) '(1 1) 1 "?" 4097); (make-cs-table 'Gen20  '(0 4096) '(3 1) 1 "?" 4097)
                          'f0 frequency
                          'freq frequency
                          'fdev 0 ;bandwidth / skirtwidth?
                          'bw (nth 6 fofdata)
                          'bwdev (nth 9 fofdata)
                          'bwenv (make-cs-table 'Gen05  '(0 4096) '(1 0.1) 3 "?" 513); (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097)
                       ;   'win (nth 7 fofdata)
                       ;   'wdur duration             ;WHAT HAPPENS IF ONLY DURATION IS APPLIED (NOT WIN AND WOUT)
                       ;   'wout (nth 8 fofdata)
                       ;   'freqjit-aenv (make-cs-table  'Gen-07  '(0 513) ' (0 0) 1 "?" 513) ;eventually defaults
                          ;'freqjit-fenv (make-cs-table  'Gen-07  '(0 513) '(50 50) 1 "?" 513)
                       ;   'bwjit-aenv (make-cs-table  'Gen-07  '(0  512) '(0 0) 1 "?" 513) ;eventually use defaults
                          ;'bwjit-fenv (make-cs-table  'Gen-07  '(0 513) '(15 15) 1 "?" 513)
                           'phs (rad->norm (sixth fofdata))
                          ; KEYWORDS
                          'magnitude themagnitude
                          'norm thenorm
                          'skirt-width (nth 9 fofdata)
                          'molecule (nth 10 fofdata)
                          )))) 
    new))

(defmethod objfromobjs ((self fof-array) (type struck-string))
  (let* ((fofdata (data self))
         (new (make-instance 'struck-string
                             :numcols (length (first fofdata))
                             :e-dels (first fofdata)
                             :durs (om* 100 (second fofdata))
                             :amp (fof-amplitude (fourth fofdata) (third fofdata))
                             :freq (fifth fofdata)
                             :balance (om/ (sixth fofdata) pi)
                             :strikevelocity (om-scale-exp (nth 9 fofdata) 20 1000 .25)
                            ; :damping (om/ 1 (om* (seventh fofdata) 0.01)) ;bandwidth-dependent!
                             :decaytime (second fofdata) ;(om-scale (seventh fofdata) 15 .5) ;(om* 50 (second fofdata)) ;(nth 7 fofdata)) ;15
                              ;:numstrings (om-round (om-scale (nth 9 fofdata) 1 5))
                              ;:detune (nth 9 fofdata)
                             )))
    new))


(defmethod objfromobjs ((self fof-array) (type johnny))
  (let* ((fofdata (data self))
         (frequency (fifth fofdata))
         (themagnitude (third fofdata))
         (thenorm (fourth fofdata))
         (duration (second fofdata))
         (new (set-array 'johnny 
                         (length (car fofdata))
                         (list 
                          'e-dels (first fofdata)
                          'durs (om* 10 (second fofdata))
                          'amp (fof-amplitude thenorm themagnitude)
                          ;'aenv (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097)
                          'carfreq frequency
                          'carfdev 0 ; (sixth fofdata)
                          'modfreq (om* 0.5 frequency)
                          'index 0 ;later possibly mapping bandwidth .. ?
                          'indexdev 0 ;later possibly mapping skirtwidth .. ?
                          'freqjit-aenv (make-cs-table  'Gen-07  '(0 513) ' (0 0) 1 "?" 513) ;eventually defaults
                          ;'freqjit-fenv (make-cs-table  'Gen-07  '(0 513) '(50 50) 1 "?" 513)
                          'indexjit-aenv (make-cs-table  'Gen-07  '(0  512) '(0 0) 1 "?" 513) ;eventually use defaults
                          ;'indexjit-fenv (make-cs-table  'Gen-07  '(0 513) '(15 15) 1 "?" 513)
                          'phi (rad->norm (sixth fofdata))
                          ; KEYWORDS
                          'magnitude themagnitude
                          'norm thenorm
                          'bandwidth (seventh fofdata)
                          'risetime (nth 7 fofdata)
                          'skirt-width (nth 9 fofdata)
                          'molecule (nth 10 fofdata)
                          )))) 
    new))

|#

;if I want to make it closer to the piano, the sounds should last longer, and the decaytime should be related to strikevelocity

(defun rad->norm (radians)
  (let* ((thenumber (om+ radians (om* 2 pi)))
         (themodnumber (second (multiple-value-list (om// thenumber 6.283185307179586)))))
         (om* themodnumber 0.159154943091895))
  )
