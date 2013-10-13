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

 
(defclass! partial-array (parametric-array)
           (
            )
           (:icon 03)
           )

; %%%% OBJFROMOBJS for Chroma classes
#|
(defmethod objfromobjs ((self partial-array) (type fm-1))
  (let* ((partialdata (data self))
         (new (make-instance 'fm-1
                             :numcols (length (first partialdata))
                             :e-dels (first partialdata)
                             :durs (second partialdata)
                             :amp (third partialdata)
                             :f0 (fifth partialdata)
                             ;:freq (fifth partialdata)
                            ; :aenv (make-cs-table 'Gen20  '(0 4096) '(3 1) 1 "?" 4097)
                            ; :ienv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)  
                             :fmod (fifth partialdata)
                             :imax (om* 62 (sixth partialdata))
                             )))
    new))

(defmethod objfromobjs ((self partial-array) (type fm-2))
  (let* ((partialdata (data self))
         (new (make-instance 'fm-2
                             :numcols (length (first partialdata))
                             :e-dels (first partialdata)
                             :durs (second partialdata)
                             :amp (third partialdata)
                             :freq (fifth partialdata)
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(3 1) 1 "?" 4097)
                             :ienv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)  
                             :fmod (fifth partialdata)
                             :imax (sixth partialdata) ; (om* pi) ?
                             )))
    new))

;leave these ones there for now...

(defmethod objfromobjs ((self partial-array) (type add-1))
  (let* ((partialdata (data self))
         (new (make-instance 'add-1
                             :numcols (length (first partialdata))
                             :e-dels (first partialdata)
                             :durs (second partialdata)
                             :amp (third partialdata)
                             :freq (fifth partialdata)
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097)
                             )))
    new))

(defmethod objfromobjs ((self partial-array) (type add-a1))
  (let* ((partialdata (data self))
         (new (make-instance 'add-a1
                             :numcols (length (first partialdata))
                             :e-dels (first partialdata)
                             :durs (second partialdata)
                             :amp (third partialdata)
                             :freq (fifth partialdata)
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(5 1) 1 "?" 4097)
                             :fenv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)
                             :fdev (sixth partialdata) ;(* 2 pi)
                             )))
    new))

(defmethod objfromobjs ((self partial-array) (type fm-2))
  (let* ((partialdata (data self))
         (new (make-instance 'fm-2
                             :numcols (length (first partialdata))
                             :e-dels (first partialdata)
                             :durs (second partialdata)
                             :amp (third partialdata)
                             :freq (fifth partialdata)
                             :aenv (make-cs-table 'Gen20  '(0 4096) '(3 1) 1 "?" 4097)
                             :ienv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097)  
                             :fmod (fifth partialdata)
                             :imax (sixth partialdata) ; (om* pi) ?
                             )))
    new))
|#

#|

;new methods for Prisma synthesizers

(defmethod objfromobjs ((self partial-array) (type johnny))
  (let* ((partialdata (data self))
         (thefreq (fifth partialdata))
         (thephase (sixth partialdata))
         (new (set-array 'johnny 
                         (length (car partialdata))
                         (list 
                          'e-dels (first partialdata)
                          'durs (second partialdata)
                          'amp (third partialdata)
                          'aenv (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097) ; 3 1
                          'carfreq thefreq ;(om* 3 thefreq) ;thefreq ;
                          'carfdev (om* 5 thephase)
                          'carfenv (make-cs-table 'Gen20  '(0 4096) '(6 1) 1 "?" 4097) ;swap the envelopes for carfenv and indexenv
                          'modfreq (om* .5 thefreq)
                          'index 0
                          'indexenv (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097)
                          'indexdev 3
                          'freqjit-aenv (make-cs-table  'Gen-07  '(0 256 513) '(30 10 0) 1 "?" 513)
                          'indexjit-aenv (make-cs-table  'Gen-07  '(0 256 513) '(10 100 10) 1 "?" 513)
                          'phi (rad->norm thephase)
                          ))) 
              )
         new))

(defmethod objfromobjs ((self partial-array) (type addison))
  (let* ((partialdata (data self))
         (new (set-array 'addison 
                         (length (car partialdata))
                         (list 
                          'e-dels (first partialdata)
                          'durs (second partialdata)
                          'amp (third partialdata)
                          'aenv (make-cs-table 'Gen20  '(0 4096) '(3 2) 1 "?" 4097)
                          'freq (fifth partialdata)
                          'fdev 0 ; (sixth partialdata)
                          'ampjit-aenv (make-cs-table  'Gen-07  '(0 256 512) '(0 1 0) 1 "?" 513)
                          'ampjit-fenv (make-cs-table  'Gen-07  '(0 513) '(15 15) 1 "?" 513)
                          'freqjit-aenv (make-cs-table  'Gen-07  '(0 256 513) '(10 10 10) 1 "?" 513)
                          'freqjit-fenv (make-cs-table  'Gen-07  '(0 513) '(50 50) 1 "?" 513)
                          'phi (sixth partialdata)
                          )) 
              ))
         new))

|#