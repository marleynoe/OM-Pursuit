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


(defclass! sound-atom ()
           (
            (onset     :accessor onset     :initarg :onset     :initform nil)
            (duration  :accessor duration  :initarg :duration  :initform nil)
            (magnitude :accessor magnitude :initarg :magnitude :initform nil)
            (norm      :accessor norm      :initarg :norm :initform nil)
            )            
           (:icon 02)
           )

(defclass! parametric-atom (sound-atom)
           (
            (frequency :accessor frequency :initarg :frequency :initform nil)
            (phi       :accessor phi       :initarg :phi       :initform nil)
            (bandwidth :accessor bandwidth :initarg :bandwidth :initform nil)
            )            
           (:icon 02)
           )

(defclass! soundgrain-atom (sound-atom)
           (
            (corpus-index :accessor corpus-index :initarg :corpus-index :initform nil)
            (file-index  :accessor file-index  :initarg :file-index  :initform nil)           
            (filepath  :accessor filepath :initarg :filepath :initform nil)
            )            
           (:icon 02)
           )


;%%%%%%%%%%%%%% ARRAYS %%%%%%%%%%%%%%%%

(defclass! sound-array (class-array)
           (
            (onset     :accessor onset     :initarg :onset     :initform nil)
            (duration  :accessor duration  :initarg :duration  :initform nil)                      
            (magnitude :accessor magnitude :initarg :magnitude :initform nil)
            (norm      :accessor norm      :initarg :norm :initform nil)
            )
           (:icon 03)
           )

(defclass! parametric-array (sound-array)
           (
            (frequency :accessor frequency :initarg :frequency :initform nil)
            (phi       :accessor phi       :initarg :phi       :initform nil)
            (bandwidth :accessor bandwidth :initarg :bandwidth :initform nil)
            )            
           (:icon 03)
           )

(defclass! soundgrain-array (sound-array)
           (
            (corpus-index :accessor corpus-index :initarg :corpus-index :initform nil)
            (file-index  :accessor file-index  :initarg :file-index  :initform nil)           
            (filepath  :accessor filepath :initarg :filepath :initform nil)
            )            
           (:icon 03)
           )

(defmethod get-fonde-pict ((self sound-array)) *my-bg-pict*)
(defmethod get-fonde-pict ((self sound-atom)) *my-bg-pict*)
