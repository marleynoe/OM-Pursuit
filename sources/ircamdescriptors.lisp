;************************************************************************
; OM-Pursuit, library for dictionary-based sound modelling in OpenMusic *
;      (c) 2011-2013 Marlon Schumacher (CIRMMT/McGill University)       *     
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


(defmethod! ircamdescriptors ((path-to-exec pathname) (path-to-audio pathname) (path-to-config pathname))
            :icon 30
            (let* ((exepath (namestring path-to-exec))
                  (audiopath (namestring path-to-audio))
                  (configpath (namestring path-to-config))
                  (outfile (om-make-pathname :directory (pathname-directory audiopath) 
                                             :name (pathname-name audiopath) 
                                             :type (format nil "~a.descr.sdif" (pathname-type audiopath))))
                  (thestring (format nil "~a ~s ~a" exepath audiopath configpath)))
              
              (print outfile)
              (om-cmd-line thestring *sys-console*)

         ;make an sdiffie object
              (let ((myoutfile (probe-file outfile))
                    (mysdifobj (load-sdif-file outfile)))         
                  ;optional removal of sdif file
                      (add-tmp-file myoutfile) 
                      ;(when *delete-inter-file* (om-run-process "cleantempfiles" #'sleep&clean 1)) ; could be a higher number...
                      mysdifobj)
              ))


(defmethod! ircamdescriptors ((path-to-exec pathname) (path-to-audio list) (path-to-config pathname))
            (mapcar #'(lambda (audiofiles)
                        (ircamdescriptors path-to-exec audiofiles path-to-config)) path-to-audio)
            )



#|
("amplitudemodulationinfo" "IMOD") ("amplitudemodulationampinfo" "IMDA") ("amplitudemodulationfreqinfo" "IMDF") ("deltainfo" "IODO") ("deltadeltainfo" "IOAO") ("deltadeltamedianfilterinfo" "IOAM") ("deltamedianfilterinfo" "IODM") ("effectivedurationinfo" "IEFD") ("logattacktimeinfo" "ILAT") ("medianfilterinfo" "IMED") ("shorttermfeatureinfo" "IDSC") ("temporalcentroidinfo" "ITCN") ("temporaldecreaseinfo" "ITDE") ("temporalincreaseinfo" "ITIN") ("weightedmeaninfo" "IWMN") ("weightedmeandeltainfo" "IMDO") ("weightedmeandeltadeltainfo" "IMAO") ("weightedmeandeltadeltamedianfilterinfo" "IMAM") ("weightedmeandeltamedianfilterinfo" "IMDM") ("weightedmeanmedianfilterinfo" "IMOM") ("weightedstddeviationinfo" "IWSD") ("weightedstddeviationdeltainfo" "ISDO") ("weightedstddeviationdeltadeltainfo" "ISAO") ("weightedstddeviationdeltadeltamedianfilterinfo" "ISAM") ("weightedstddeviationdeltamedianfilterinfo" "ISDM") ("weightedstddeviationmedianfilterinfo" "ISOM")))
|#

(defvar *ircamdescriptor-matrixdescriptions*
'(("1ARR" "autocorrelation") ("1CHR" "chroma") ("1EEV" "energyenvelope") ("1FQ0" "fundamentalfrequency") ("1HEN" "harmonicenergy") ("1HOE" "harmonicoddtoevenratio") ("1HCN" "harmonicspectralcentroid") ("1HDE" "harmonicspectraldecrease") ("1HSD" "harmonicspectraldeviation") ("1HKU" "harmonicspectralkurtosis") ("1HRO" "harmonicspectralrolloff") ("1HSK" "harmonicspectralskewness") ("1HSL" "harmonicspectralslope") ("1HSP" "harmonicspectralspread") ("1HVA" "harmonicspectralvariation") ("1HTR" "harmonictristimulus") ("1INH" "inharmonicity") ("1LDN" "loudness") ("1MFC" "mfcc") ("1NEN" "noiseenergy") ("1NSN" "noisiness") ("1POE" "perceptualoddtoevenratio") ("1PCN" "perceptualspectralcentroid") ("1PDE" "perceptualspectraldecrease") ("1PSD" "perceptualspectraldeviation") ("1PKU" "perceptualspectralkurtosis") ("1PRO" "perceptualspectralrolloff") ("1PSK" "perceptualspectralskewness") ("1PSL" "perceptualspectralslope") ("1PSP" "perceptualspectralspread") ("1PVA" "perceptualspectralvariation") ("1PTR" "perceptualtristimulus") ("1RSL" "relativespecificloudness") ("1SHA" "sharpness") ("1ZCR" "signalzerocrossingrate") ("1SCN" "spectralcentroid") ("1SCM" "spectralcrest") ("1SDE" "spectraldecrease") ("1SFM" "spectralflatness") ("1SKU" "spectralkurtosis") ("1SRO" "spectralrolloff") ("1SSK" "spectralskewness") ("1SSL" "spectralslope") ("1SSP" "spectralspread") ("1SVA" "spectralvariation") ("1SPR" "spread") ("1NRG" "totalenergy") ("IMOD" "amplitudemodulationinfo") ("IMDA" "amplitudemodulationampinfo") ("IMDF" "amplitudemodulationfreqinfo") ("IODO" "deltainfo") ("IOAO" "deltadeltainfo") ("IOAM" "deltadeltamedianfilterinfo") ("IODM" "deltamedianfilterinfo") ("IEFD" "effectivedurationinfo") ("ILAT" "logattacktimeinfo") ("IMED" "medianfilterinfo") ("IDSC" "shorttermfeatureinfo") ("ITCN" "temporalcentroidinfo") ("ITDE" "temporaldecreaseinfo") ("ITIN" "temporalincreaseinfo") ("IWMN" "weightedmeaninfo") ("IMDO" "weightedmeandeltainfo") ("IMAO" "weightedmeandeltadeltainfo") ("IMAM" "weightedmeandeltadeltamedianfilterinfo") ("IMDM" "weightedmeandeltamedianfilterinfo") ("IMOM" "weightedmeanmedianfilterinfo") ("IWSD" "weightedstddeviationinfo") ("ISDO" "weightedstddeviationdeltainfo") ("ISAO" "weightedstddeviationdeltadeltainfo") ("ISAM" "weightedstddeviationdeltadeltamedianfilterinfo") ("ISDM" "weightedstddeviationdeltamedianfilterinfo") ("ISOM" "weightedstddeviationmedianfilterinfo"))
)

(defparameter *ircamdescriptortypes*
  '(("signalzerocrossingrate" "1ZCR") ("spectralcentroid" "1SCN") ("spectralcrest" "1SCM") ("spectraldecrease" "1SDE") ("spectralflatness" "1SFM") ("spectralkurtosis" "1SKU") ("spectralrolloff" "1SRO") ("spectralskewness" "1SSK") ("spectralslope" "1SSL") ("spectralspread" "1SSP") ("spectralvariation" "1SVA") ("spread" "1SPR") ("totalenergy" "1NRG")("autocorrelation" "1ARR") ("chroma" "1CHR") ("energyenvelope" "1EEV") ("fundamentalfrequency" "1FQ0") ("harmonicenergy" "1HEN") ("harmonicoddtoevenratio" "1HOE") ("harmonicspectralcentroid" "1HCN") ("harmonicspectraldecrease" "1HDE") ("harmonicspectraldeviation" "1HSD") ("harmonicspectralkurtosis" "1HKU") ("harmonicspectralrolloff" "1HRO") ("harmonicspectralskewness" "1HSK") ("harmonicspectralslope" "1HSL") ("harmonicspectralspread" "1HSP") ("harmonicspectralvariation" "1HVA") ("harmonictristimulus" "1HTR") ("inharmonicity" "1INH") ("loudness" "1LDN") ("mfcc" "1MFC") ("noiseenergy" "1NEN") ("noisiness" "1NSN") ("perceptualoddtoevenratio" "1POE") ("perceptualspectralcentroid" "1PCN") ("perceptualspectraldecrease" "1PDE") ("perceptualspectraldeviation" "1PSD") ("perceptualspectralkurtosis" "1PKU") ("perceptualspectralrolloff" "1PRO") ("perceptualspectralskewness" "1PSK") ("perceptualspectralslope" "1PSL") ("perceptualspectralspread" "1PSP") ("perceptualspectralvariation" "1PVA") ("perceptualtristimulus" "1PTR") ("relativespecificloudness" "1RSL") ("sharpness" "1SHA"))) 

#|
(defvar *ircamdescriptortypes*
  (loop for item in *ircamdescriptor-matrixdescriptions*
        collect (reverse item)
        ))
|#


(defmethod! desc->sig ((desc t))
            :menuins (list (list 0 *ircamdescriptortypes*))
            desc)
            

(defvar *ircam-matrixtypedefs*
'((itin temporalincreaseinfo) (1eev energyenvelope) (1hcn harmonicspectralcentroid) (1hen harmonicenergy) (isom weightedstddeviationmedianfilterinfo) (1hoe harmonicoddtoevenratio) (1mfc mfcc) (1ldn loudness) (iwmn weightedmeaninfo) (1inh inharmonicity) (iwsd weightedstddeviationinfo) (1hsd harmonicspectraldeviation) (1hku harmonicspectralkurtosis) (1hva harmonicspectralvariation) (1pde perceptualspectraldecrease) (1nen noiseenergy) (1hsk harmonicspectralskewness) (1hsl harmonicspectralslope) (1hro harmonicspectralrolloff) (1pcn perceptualspectralcentroid) (1hsp harmonicspectralspread) (1sde spectraldecrease) (1htr harmonictristimulus) (1sha sharpness) (1scm spectralcrest) (1scn spectralcentroid) (1poe perceptualoddtoevenratio) (1sfm spectralflatness) (iefd effectivedurationinfo) (1psd perceptualspectraldeviation) (1pku perceptualspectralkurtosis) (1nsn noisiness) (1pva perceptualspectralvariation) (1psk perceptualspectralskewness) (1psl perceptualspectralslope) (1pro perceptualspectralrolloff) (1psp perceptualspectralspread) (1sku spectralkurtosis) (1sva spectralvariation) (1ptr perceptualtristimulus) (1rsl relativespecificloudness) (idsc shorttermfeatureinfo) (1ssk spectralskewness) (1spr spread) (1ssl spectralslope) (imda amplitudemodulationampinfo) (1sro spectralrolloff) (1zcr signalzerocrossingrate) (1ssp spectralspread) (imdf amplitudemodulationfreqinfo) (imed medianfilterinfo) (imam weightedmeandeltadeltamedianfilterinfo) (imao weightedmeandeltadeltainfo) (ilat logattacktimeinfo) (imdm weightedmeandeltamedianfilterinfo) (imdo weightedmeandeltainfo) (ioam deltadeltamedianfilterinfo) (ioao deltadeltainfo) (iodm deltamedianfilterinfo) (iodo deltainfo) (imod amplitudemodulationinfo) (isam weightedstddeviationdeltadeltamedianfilterinfo) (isao weightedstddeviationdeltadeltainfo) (itde temporaldecreaseinfo) (imom weightedmeanmedianfilterinfo) (isdm weightedstddeviationdeltamedianfilterinfo) (isdo weightedstddeviationdeltainfo) (itcn temporalcentroidinfo) (1hde harmonicspectraldecrease) (1chr chroma))
)