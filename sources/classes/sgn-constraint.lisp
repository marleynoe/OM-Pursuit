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

(defvar *pursuit-constraint-sdif-types* 
  (x-append 
   (make-instance 'sdiftype
                  :struct 'f
                  :signature "XPCT"
                  :description '(("XPCT" "Pursuit-Constraint") ("XMSA" "Max-simultaneous-atoms") ("XMSC" "Max-simultaneous-corpus-atoms") ("XMNT" "Min-time-distance") ("XMXT" "Max-time-distance")) 
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XPCT"
                  :description '("Pursuit-Constraint")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMSA"
                  :description '("Max-simultaneous-atoms")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMSC"
                  :description '("Max-simultaneous-corpus-atoms")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMNT"
                  :description '("Min-time-distance")
                  )
   (make-instance 'sdiftype
                  :struct 'm
                  :signature "XMXT"
                  :description '("Max-time-distance")
                  )
   ))

(defvar *om-pursuit_default-nvt* (make-instance 'sdifnvt
                                                :nv-pairs '(("This file was produced by" "OM-Pursuit") ("Authors" "Marlon Schumacher, Graham Boyes"))
                                                :tablename "OM-Pursuit Info"
                                                :id 0 ))

; class for defining a constraint in OM-Pursuit
; how can I make menuins for slots of the classes?

(defclass! sgn-constraint ()
           (
            (constraint :accessor constraint :initarg :constraint :initform nil :type symbol)
            (descriptor :accessor descriptor :initarg :descriptor :initform nil)
            (order :accessor order :initarg :order :initform nil)
            (value :accessor value :initarg :value :initform nil)
            (streamid :accessor streamid :initarg :streamid :initform nil)
            )
           (:icon 02)
           )

