;=============================================================================
;		SMPL-LINEAR.ORC
; SAMPLER READING FROM A SOUND FILE WITH AUTOMATIC SR CONVERSION / MONO
; READING SAMPLES THROUGH DISKIN2, NO LOOP
; CONTROLLABLE WRAP
; AMPLITUDE ENVELOPE WITH POSCILI
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
;-----------------------------------------------------------------------------
;	p1	= instrument number
;	p2	= action time [sec]
;	p3	= duration [sec]
;	p4	= max amp [linear, >0.0-1000.0 or dB, <= 0.0]
;	p5	= transposition factor [1=same as original]
;	p6	= sound file [name]
;	p7	= starting point in file [sec]
;	p8	= amp envelope [GEN]
;	p9	= wrap [0 or non 0]
;-----------------------------------------------------------------------------
; COMPULSORY GEN FUNCTIONS :
;	f19	sigmoid rise/decay
;_____________________________________________________________________________

; CLASS: SMPL-1

;  GLOBAL KEYWORDS (default values within parentheses):
;	NUMROWS		: amount of rows (components) in the event (1)
;	ACTION-TIME	: start time of the whole event [sec] (0.0)
;	USER-FUN	: user-defined parsing function (nil)

;  LOCAL KEYWORDS:
;   E-DELS	: entry delays [sec] (0.0)
;	DURS	: duration [sec] (1.0)
;	AMP		: max amplitude [lin, >0.0-1000.0 or dB <- 0.0] (0.0)
;	F0  	: transposition factor [1=same freq] (1.0)
;	AFIL	: file name [int, sound, string, pathname or GEN] (santuri_96.aif)
;	SKIP	: starting point in the file [sec] (0.0)
;			  if < 0, it will add -(SKIP/XPF) seconds of delay
;	AENV	: amplitude envelope [GEN] (triangle)
;	WRAP	: if=0, locations that are beyond the file produce silence,
;			  if non 0, they are wrapped to the duration of the sound (1)
;*****************************************************************************

sr	= 96000
kr	= 96000
ksmps	= 1
nchnls	= 1

;0dbfs = 32767  ; 16 bits
0dbfs = 8388607 ; max amp of the sound file

instr 1 ; -------------------------------------------------------------
 
idur		= p3
idurosc	= 1/idur
iamp 		= p4
	
if0 		= p5 ; transposition factor
ifile		= p6

iskip		= (p7 < 0.0 ? (p7*if0) : p7)
	; to make the value not depend on the xposition factor if it's negative
iaenv		= p8	; global amplitude envelope

ixmode	= 1 ; index between 0 and 1
ixrap		= 0 ; no wraparound in table reading
iwrap		= p9

iformat		= 8 ; 24-bit int, ignored if the sound file has a header
iwsize		= 512 ; the bigger, the better the quality of the transposition
ibufsize		= 262144	; maximum = 1048576, higher makes less disk access
iskipinit		= 0 ; do not skip initialization

; global envelope
k3	poscil	iamp, idurosc, iaenv

; file reading
a1	diskin2	ifile, if0, iskip, iwrap, iformat, iwsize, ibufsize, iskipinit
asound = a1*k3
;OUTBEG
         outc     asound
;OUTEND

endin

