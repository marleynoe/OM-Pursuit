;************************************************************************
; OM-Pursuit, library for dictionary-based sound modelling in OpenMusic *
;      (c) 2011-2014 Marlon Schumacher (CIRMMT/McGill University)       *     
;               https://github.com/marleynoe/OM-Pursuit                 *
;                                                                       *
;                DSP based on pydbm - (c) Graham Boyes                  *
;                  https://github.com/gboyes/pydbm                      *
;************************************************************************
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


; perhaps call this 'build-database' or something more intuitive
; sdiftypes should be given in the function already! as a variable (defvar)
; compat


; this function for now works without calling the ircamdescriptors (i.e. only using duration)
(defmethod! pursuit-dictionary ((soundgrain-paths list) (sdifpath pathname) &key sdiftypes add-frames)
            :icon 05
            :initvals '(nil nil nil nil)
            :indoc '("1" "path for resulting output sdiffile" "list of types for sdif frame+matrixtypes to be included in sdiffile" "add additional custom sdif frames")
            :doc "The OM-Pursuit dictionary building function"
            :outdoc '("dictionary as sdif file")
            (let ((sid-list (flat
                             (loop for path in soundgrain-paths
                                   for i from 1 to (length soundgrain-paths) collect
                                  (make-instance 'sdifsid :id i :source (namestring path) :treeway (makestring 1))) ; all paths are corpus = 1?
                                     ))
                  (sdifframelist (flat
                                  (loop for path in soundgrain-paths
                                   for i from 1 to (length soundgrain-paths) collect
                                   (let* ((glbmatrix ;(make-instance 'sdifmatrix :numcols 1 :signature "XDUR" :k0 :duration (sound-dur path))) 
                                           (make-instance 'raw-SDIFMatrix :signature "XDUR" :num-elts 1 :num-fields 1 :data (list (sound-dur path))))
                                          (glbframe (make-instance 'sdifframe :signature "XGLB" :ftime 0 :streamid i :lmatrix glbmatrix))
                                          (crpmatrix ;(make-instance 'sdifmatrix :numcols 1 :signature "XCRP" :k0 :corpus-index 1))
                                           (make-instance 'raw-SDIFMatrix :signature "XCRP" :num-elts 1 :num-fields 1 :data '(1)))
                                          (crpframe (make-instance 'sdifframe :signature "XGLB" :ftime 0 :streamid i :lmatrix crpmatrix)))
                                     (x-append glbframe crpframe)))
                                  ))
                  (thesdiftypes sdiftypes))
              (make-instance 'sdif-buffer :types thesdiftypes :nvts sid-list :lframes sdifframelist)
              ;(list sid-list sdifframelist)
            ))
; this will need methods for lists of lists (to write the corpora) -> or otherwise when provided as flattended list? no corpus distinction!

#|
;needs something like this for the header:
(defvar *spat-sdiftypes* nil)

(setf *spat-sdiftypes* 
      (reduce #'(lambda (s1 s2) (string+ s1 " " s2))
              (list "{"
                    "1MTD XCAR {x, y, z}" 
                    "1MTD XPOS {x, y, z}" 
                    "1MTD XAED {azimuth, elevation, distance}"
                    "1MTD XORI {yaw, pitch, roll}" 
                    "1MTD XAPE {aperture}" 
                    "1MTD PRES {presence}"
                    "1MTD WARM {warmth}"
                    "1MTD BRIL {brillance}"
                    "1MTD PRER {room_presence}"
                    "1MTD REVP {running_reverberance}"
                    "1MTD ENVP {envelopment}"
                    "1MTD OMNI {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD AXIS {global_gain, gain_low, gain_mid, gain_high, freq_low, freq_high}"
                    "1MTD XRID {room_index}"
                    
                    "1MTD XDEC {rt_global, rt_low, rt_mid, rt_high, freq_low, freq_High}"	
                    "1MTD XRIR {early_start, early_end, cluster_start, cluster_end, reverb_start, modal_density, early_distr, cluster_distr}"
                    
                    "1FTD XSRC {XCAR cartesian_coordinates; XAED navigational_coordinates; XORI orientation; XAPE aperture; PRES presence; WARM warmth; BRIL brillance; PRER room_presence; REVP running_reverberance; ENVP envelopment; OMNI omni_filter; AXIS axis_filter; XRID room_index;}"

                    "1FTD XRFX {XDEC decay_times; XRIR time_response;}"
                    
                    "}")))
|#