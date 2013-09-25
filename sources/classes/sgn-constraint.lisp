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
  (x-append (make-instance 'sdiftype
                           :struct 'f
                           :signature "XPCT"
                           :description '(("XPCT" "Pursuit-Constraint"))
                           )
            (make-instance 'sdiftype
                           :struct 'm
                           :signature "XPCT"
                           :description '("Pursuit-Constraint")
                           )
            ))

(defvar *om-pursuit_default-nvt* (make-instance 'sdifnvt
                                                :nv-pairs '(("This file was produced by" "OM-Pursuit") ("Author" "M.Schumacher,G.Boyes"))
                                                :tablename "OM-Pursuit Info"
                                                :id 0 )
  )


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

(defmethod! sgn-constraint-p ((self t)) nil)
(defmethod! sgn-constraint-p ((self sgn-constraint)) t)

;function that will return an instance of class 'sgn-constraint

(defmethod! sgn-constraint-maker ((constraint symbol) (descriptor string) (order integer) (value t))
            ; :icon 
            :initvals '(nil "0" 0 nil)
            :menuins '( (0 ( ("<" '<) ("<=" '<=) (">" '>) (">=" '>=)  ("==" '==) ("!=" '!=)  ("W" 'W) ("B" 'B) ))
                        (1 ( ("SpectralCentroid" "1SCN") ("SpectralKurtosis" "1SLK") ("SpectralRolloff" "1SLR") ("SpectralDecrease" "1SPD") )))
            :indoc '("constraint as a symbol" "which descriptor to apply the constraint to" "an integer specifying the order" "the value for the constraint (static = number, dynamic = bpf)")
            :doc "Defines and returns an instance of sgn-constraint"
            (make-instance 'sgn-constraint
                           :constraint constraint
                           :descriptor descriptor
                           :order order
                           :value value)
            )

(defmethod! sgn-constraint-maker ((constraint integer) (descriptor string) (order integer) (value t))
            (make-instance 'sgn-constraint
                           :constraint constraint
                           :descriptor descriptor
                           :order order
                           :value value)
            )

; this function allows combining different sub-constraints (constraint-blocks) into larger scale tree-structures
(defmethod! ctr-combine ((operator symbol) (constraint1 t) (constraint2 t))
            :initvals '(nil nil nil)
            :menuins '( (0 (("AND" 'AND) ( "NAND" 'NAND ) ( "OR" 'OR ) ( "NOR" 'NOR ) ( "XOR" 'XOR ) ( "XNOR" 'XNOR ) ( "W" 'W ) ( "B" 'B )))) ; should be AND NAND OR NOR XOR XNOR
            ;need a check here: 
            ;(print (constraint constraint1))
            ;(print (constraint constraint2)) ; need to be able to check for nested constraints, possibly with a cond statement
            ;(if ((or (consp (constraint constraint1)) (consp (constraint constraint1)
            #|
            (cond  ((or (equal (constraint constraint1) 'w) (equal (constraint constraint2) 'w)) (list constraint1 'w constraint2))
                   ((or (equal (constraint constraint1) 'b) (equal (constraint constraint2) 'b)) (list constraint1 'b constraint2))
                   (t (list constraint1 operator constraint2)))
            |#
            (list constraint1 operator constraint2)
            )

; this function takes a list of sgn-constraints and operators and writes an SDIF files
(defmethod! make-constraint-sdif ((constraint-list t))
            ;(print constraint-list)
            (let* ((object-list (flat (list-filter 'sgn-constraint-p (list! constraint-list) 'pass)))
                   (side-effect (loop for item in object-list 
                                      for i from 1 to (length object-list) do
                                      (setf (streamid item) i)
                                      )) 
                   (full-constraint (replace-ctr-with-str constraint-list))
                   (frame-list
                    (loop for item in object-list 
                          for i from 1 to (length object-list) collect
                                
                          (list
                           ;write StreamID table
                           (make-instance 'sdifsid
                                          :id i
                                          :source (string+ "constraint-" (number-to-string i))
                                          :treeway (format nil " ~a ~a ~d ~d" (constraint item) (descriptor item) (order item) (streamid item)) ;reduce? ;(makestring (replace-ctr-with-str item))
                                          )
                           
                           ; write SDIF frames
                           (if (bpf-p (value item))
                               (loop for pair in (point-pairs (value item)) collect
                                     (make-instance 'sdifframe
                                                    :signature "XPCT"
                                                    :ftime (car pair)
                                                    :StreamId i
                                                    :LMatrix (make-instance 'raw-sdifmatrix
                                                                            :signature (print "XPCT")
                                                                            :data (list (second pair))
                                                                            )
                                                    )
                                     )
                             (make-instance 'sdifframe
                                            :signature "XPCT"
                                            :ftime 0
                                            :StreamId (print i)
                                            :LMatrix  (make-instance 'raw-sdifmatrix
                                                                            :signature (print "XPCT")
                                                                            :data (list (value item))
                                                                            )
                                            )
                             ))
                          ))
                   
                   ; write nvt
                   (nvt (make-instance 'sdifnvt
                                       :nv-pairs (list (list "Fullconstraint" (makestring full-constraint)))
                                       :tablename "Constraint"
                                       :id 0))
                   (transframes (mat-trans frame-list)))
              
              ; output
              ;(list (x-append nvt (car transframes)) (flat (cdr transframes)))
              
              (make-instance 'sdif-buffer
                             :types *pursuit-constraint-sdif-types*
                             :nvts (x-append *om-pursuit_default-nvt* nvt (car transframes))
                             :lframes (flat (cdr transframes))
                             )   
              ))

; why can't I set the signature of an sdifmatrix?
(signature (make-instance 'sdifmatrix
                          :numcols 2
                          :signature (print "XPCT")
                                                                         ;:par1 (value item)
                          ))

; this function takes a constraint-tree and replaces the objects with values of the slots

(defmethod! replace-ctr-with-str ((self sgn-constraint))
            ;(format nil "( ~a ~a ~d ~d )" (constraint self) (descriptor self) (order self) (streamid self))
            (list (constraint self) (descriptor self) (order self) (streamid self))
            )

(defmethod! replace-ctr-with-str ((self t))
            self
            )

(defmethod! replace-ctr-with-str ((self list))
            (mapcar #'(lambda (x) (replace-ctr-with-str x)) self)
            )

