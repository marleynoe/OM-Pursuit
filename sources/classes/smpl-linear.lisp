;******************************************************************
;		     CLASS SMPL-1
;******************************************************************

(in-package :om)

; LISP-DEFINED CLASSES SHOULD RESIDE IN THE LIBRARY'S PACKAGE AND 
;   NOT IN THE USER PACKAGE, WHICH CONTAINS ALL THE CLASSES
;   GRAPHICALLY DEFINED


(defclass! smpl-linear
  (cs-evt) 			; INHERIT FROM CS-EVT
  (

; GLOBAL SLOTS (LIGHT BLUE, ON THE LEFT OF THE CLASS):
;    THE METHOD BELOW TRANSFORMS THEM INTO GLOBAL SLOTS ("SHOW" UNCHEKED)
;    ATTENTION: A GLOBAL SLOT SHOULD NOT HAVE AN INITARG

   ( source-code :initform
                 (load-buffer-textfile
                  (get-orc-source (get-orc "smpl-linear"))
                  'textfile "append")
                 :allocation :class
                 :type textfile
                 :accessor source-code)
   ( numchan :initform (or (get-orc-channels (get-orc "smpl-linear")) 1)
             :allocation :class  :accessor numchan)
   
   (cs-inits :initform (get-cs-inits (get-orc "smpl-linear")) 
             :allocation :class :type list :accessor cs-inits)
   
   (orc-header :initform (list
                          "; GEN functions **********************************************************"
                          "; sigmoid rise/decay"
                          "f19 0  65537  19 .5 .5 270 .5"
                          ) 
               :allocation :class :type list :accessor orc-header)

   (InstID :initform 1  :allocation :class  :accessor InstID)

; LOCAL SLOTS (RED, CORRESPONDING TO THE P-FIELDS)
;    ATTENTION: A GLOBAL SLOT SHOULD HAVE AN INITARG

  ( amp		:type number
		:initarg :amp 
  		:initform 0.0
		:accessor amp)

   ( f0 	:type number
        	:initarg :f0
        	:initform 1.0
        	:accessor f0)

   ( afil	:type t
        	:initarg :afil 
        	:initform (infile "santuri_96.aif" :subdirs '("Snd"))
        	:accessor afil)

   ( skip	:type number
        	:initarg :skip 
        	:initform 0.0
        	:accessor skip)

   ( aenv	:type cs-table
		:initarg :aenv 
                                               ; x-points y-points decimals
  		:initform (make-cs-table  'Gen07  '(0 2048 4096) '(0 100 0) 1 "?" 4097)
		:accessor aenv)

   ( wrap	:type number
        	:initarg :wrap 
        	:initform 1
        	:accessor wrap)
   )

  (:documentation
   "
;=============================================================================
;		SMPL-LINEAR.ORC
; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
; READING SAMPLES THROUGH DISKIN2, NO LOOP
; CONTROLLABLE WRAP
; AMPLITUDE ENVELOPE WITH POSCILI + COSINE IN-OUT
; FIXED TRANSPOSITION
;=============================================================================

; Timbre:       Reading from a sound file, with transposition
; Synthesis:    Sampler
; Coded:     	ms 3/09

; NB: NEW STRUCTURE FOR THE AMPLITUDES FROM AUGUST 2008!
;    Positive value > 0.0  : linear amplitude (>0.0-1000.0)
;    0.0 or negative value : amplitude in dB (0 = maximum value)

; The apparently arbitrary amplitude range (0-1000, rather than 0-1)
;     avoids printing small values with exponential notation
; Default SR = 96000, recommended precision: 24 bits

;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp
;	p5	= transposition factor [1=same freq as the file]
;	p6	= sound file [name]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN, straight line]
;	p9	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
"
   )
  (:icon 1008)
  )
