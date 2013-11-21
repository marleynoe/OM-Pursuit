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


(defmethod! get-specific-streams (sdiffile) &key ;(sdiffile list-of-streamIDs) -> for now static
  (let ((thestreams (sdifstreams sdiffile)))
    (table-filter #'(lambda (mystream) 
                      (if  (or (equal mystream 1) (equal mystream 2)) t nil))
                  thestreams
                  0
                  'pass)
    ))
         

(defun next-frame-is-ok (ptr)
 (good-signature-p (sdif::SdifSignatureToString (sdif::SdifFCurrSignature ptr))))

(defun get-sdif-file-data (sdiffile mat-types)
 (let* ((path (filepathname sdiffile))
        (fileptr (sdif-open-file path :eReadFile))
       (data nil))
   (when (and fileptr (not (sdif-null-ptr-p fileptr)))
     (sdif::SdifFReadGeneralHeader fileptr)
     (sdif::SdifFReadAllASCIIChunks fileptr)
     (loop while (next-frame-is-ok fileptr) do
           (multiple-value-bind (sig time id pos nbmat)
               (read-frame-header fileptr)
             (if (string-equal sig "1WMN")
               (push 
                ;(list time
                      (remove nil
                              (loop for matnum from 0 to (1- nbmat) collect
                                    (multiple-value-bind (sig nrows ncols size pos)
                                        (read-matrix-header fileptr) 
                                      (if (find sig mat-types :test 'string-equal)
                                   (prog1
                                       (loop for k from 0 to (1- nrows)
                                             do (sdif::SdifFReadOneRow fileptr) ; can be optimized by loading the full matrix
                                             collect (loop for n from 1 to ncols collect
                                                           (sdif::SdifFCurrOneRowCol fileptr n)))
                                     (sdif::SdifFReadPadding fileptr (sdif-calculate-padding nrows ncols size)))
                                        (progn (sdif::SdifFSkipMatrixData fileptr) nil))
                                      )))
                      ;)
                      data)
               (sdif::SdifFSkipFrameData fileptr))
           (sdif-get-signature fileptr)))
     (sdif-close-file fileptr))
   (reverse (remove nil data)))) ;maybe remove nil here and/or time
   
(defvar *ircamdescriptors-sdif-matrix-types*
'((itin {temporalincreaseinfo}) (1eev {energyenvelope}) (1hcn {harmonicspectralcentroid}) (1hen {harmonicenergy}) (isom {weightedstddeviationmedianfilterinfo}) (1hoe {harmonicoddtoevenratio}) (1mfc {mfcc}) (1ldn {loudness}) (iwmn {weightedmeaninfo}) (1inh {inharmonicity}) (iwsd {weightedstddeviationinfo}) (1hsd {harmonicspectraldeviation}) (1hku {harmonicspectralkurtosis}) (1hva {harmonicspectralvariation}) (1pde {perceptualspectraldecrease}) (1nen {noiseenergy}) (1hsk {harmonicspectralskewness}) (1hsl {harmonicspectralslope}) (1hro {harmonicspectralrolloff}) (1pcn {perceptualspectralcentroid}) (1hsp {harmonicspectralspread}) (1sde {spectraldecrease}) (1htr {harmonictristimulus}) (1sha {sharpness}) (1scm {spectralcrest}) (1scn {spectralcentroid}) (1poe {perceptualoddtoevenratio}) (1sfm {spectralflatness}) (iefd {effectivedurationinfo}) (1psd {perceptualspectraldeviation}) (1pku {perceptualspectralkurtosis}) (1nsn {noisiness}) (1pva {perceptualspectralvariation}) (1psk {perceptualspectralskewness}) (1psl {perceptualspectralslope}) (1pro {perceptualspectralrolloff}) (1psp {perceptualspectralspread}) (1sku {spectralkurtosis}) (1sva {spectralvariation}) (1ptr {perceptualtristimulus}) (1rsl {relativespecificloudness}) (idsc {shorttermfeatureinfo}) (1ssk {spectralskewness}) (1spr {spread}) (1ssl {spectralslope}) (imda {amplitudemodulationampinfo}) (1sro {spectralrolloff}) (1zcr {signalzerocrossingrate}) (1ssp {spectralspread}) (imdf {amplitudemodulationfreqinfo}) (imed {medianfilterinfo}) (imam {weightedmeandeltadeltamedianfilterinfo}) (imao {weightedmeandeltadeltainfo}) (ilat {logattacktimeinfo}) (imdm {weightedmeandeltamedianfilterinfo}) (imdo {weightedmeandeltainfo}) (ioam {deltadeltamedianfilterinfo}) (ioao {deltadeltainfo}) (iodm {deltamedianfilterinfo}) (iodo {deltainfo}) (imod {amplitudemodulationinfo}) (isam {weightedstddeviationdeltadeltamedianfilterinfo}) (isao {weightedstddeviationdeltadeltainfo}) (itde {temporaldecreaseinfo}) (imom {weightedmeanmedianfilterinfo}) (isdm {weightedstddeviationdeltamedianfilterinfo}) (isdo {weightedstddeviationdeltainfo}) (itcn {temporalcentroidinfo}) (1hde {harmonicspectraldecrease}) (1chr {chroma}))
)
          
(defvar *new-sdiftypes* 
"
SDIF

1TYP
{
1MTD 1NVT { NVTText }
1MTD 1TYP { TYPText }
1MTD 1IDS { IDSText }
1FTD 1NVT { 1NVT NameValueTable;  }
1FTD 1TYP { 1TYP TypeDefinitions;  }
1FTD 1IDS { 1IDS StreamInfo;  }

1MTD 1GAI { Gain }
1FTD 1GAI { 1GAI Gain;  }
1MTD IWIN { WindowIdentifier, WindowSize }
1MTD 1WIN { Samples }
1FTD 1WIN { IWIN WindowInfo; 1WIN Window;  }
1MTD 1CHA { Channel1, Channel2 }

1MTD 1FQ0 { Frequency, Confidence, Score, RealAmplitude }
1FTD 1FQ0 { 1FQ0 FundamentalFrequencyEstimate;  }

1MTD 1PIC { Frequency, Amplitude, Phase, Confidence }
1MTD 1TRC { Index, Frequency, Amplitude, Phase }
1MTD 1HRM { Index, Frequency, Amplitude, Phase }
1FTD 1PIC { 1PIC PickedPeaks;  }
1FTD 1TRC { 1TRC SinusoidalTracks;  }
1FTD 1HRM { 1HRM HarmonicPartials;  }
1MTD 1HRE { MeanDeltaFrequency, Harmonicity, WeightedHarmonicity }
1FTD 1HRE { 1HRE HarmonicityEstimate;  }

1MTD IENV { HighestBinFrequency, ScaleType, BreakFrequency }
1MTD 1ENV { Env }
1FTD 1ENV { IENV SpectralEnvelopeInfo; 1ENV SpectralEnvelope; 1GAI Gain;  }
1MTD ITFC { SamplingRate, Order }
1MTD 1CEC { CepstralCoefficients }
1FTD 1CEC { 1CEC CepstralCoefs;  }
1MTD 1ARA { AutoRegressiveCoefficients }
1MTD 1ARK { ReflectionCoefficients }
1MTD 1ARR { AutoCorrelationCoefficients }
1FTD 1ARA { 1GAI Gain; 1ARA ARACoefs;  }
1FTD 1ARK { 1GAI Gain; 1ARK ARKCoefs;  }
1FTD 1ARR { 1ARR ARRCoefs;  }

1MTD 1FOF { Frequency, Amplitude, BandWidth, Tex, DebAtt, Atten, Phase }
1MTD 2RES { Frequency, Amplitude, DecayRate, Phase }
1MTD 1RES { Frequency, Amplitude, BandWidth, Saliance, Correction }
1MTD 1DIS { Distribution, Amplitude }
1FTD 1NOI { 1DIS NoiseDistribution;  }
1FTD 1FOB { 1FQ0 FundamentalFrequencyEstimate; 1FOF Formants; 1CHA Channels;  }
1FTD 1REB { 1RES Filters; 1CHA Channels;  }

1MTD ISTF { DFTPeriod, WindowDuration, FFTSize }
1MTD 1STF { Real, Imaginary }
1FTD ISTF { ISTF FourierTransformInfo;  }
1FTD 1STF { ISTF FourierTransformInfo; 1STF FourierTransform; 1WIN Window;  }

1MTD INRG { Scale, NormalisationFactor }
1MTD 1NRG { Energy }
1FTD 1NRG { INRG ScaleAndFactor; 1NRG Energy; IWIN WindowInfo; 1WIN Window;  }
1MTD 1BND { LowerFrequencyLimit, UpperFrequencyLimit }
1FTD 1BND { 1BND Bands;  }

1MTD ITDS { SamplingRate }
1MTD 1TDS { Sample }

1MTD 1PEM { Identifier, Parameter1, Parameter2, Parameter3 }
1MTD ITMR { Index, Frequency, Amplitude, Phase }
1MTD ITMI { Index }
1MTD 1BEG { Id }
1MTD 1END { Id }
1MTD 1SEG { Confidence }
1FTD 1SEG { 1SEG Segmentation;  }
1MTD 1LAB { Chars }
1FTD 1MRK { 1BEG SegmentStart; 1END SegmentEnd; 1SEG Segmentation; 1LAB Label; 1PEM PeriodMarker; ITMR TransientMarkerRepresentation; ITMI TransientMarkerIdentifier;  }

1MTD 1VUN { VoicingCoefficient }
1MTD 1VUF { CuttingFrequency }
1FTD 1VUV { 1VUN VoicedUnvoicedNorm; 1VUF VoicedUnvoicedFreq;  }

1MTD 1MID { Status, Data1, Data2 }
1MTD 1SYX { Data }
1FTD 1MID { 1MID MIDIEvent; 1SYX MIDISystemExclusive;  }

1MTD EMPM { Value, Index }
1MTD EMJR { Record }
1FTD EFPM { EMPM Tableau; EMJR EndRecording;  }

1MTD 1SCN { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1SCN { 1SCN SpectralCentroid;  }
1MTD 1SSP { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1SSP { 1SSP SpectralSpread;  }
1MTD 1SSK { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1SSK { 1SSK SpectralSkewness;  }
1MTD 1SKU { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1SKU { 1SKU SpectralKurtosis;  }
1MTD 1SSL { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1SSL { 1SSL SpectralSlope;  }
1MTD 1SDE { Value }
1FTD 1SDE { 1SDE SpectralDecrease;  }
1MTD 1SRO { Value }
1FTD 1SRO { 1SRO SpectralRollOff;  }
1MTD 1SVA { lin-ampl, power-ampl, log-ampl }
1FTD 1SVA { 1SVA SpectralVariation;  }

1MTD 1NSN { Value }
1FTD 1NSN { 1NSN Noisiness;  }
1MTD 1INH { Value }
1FTD 1INH { 1INH Inharmonicity;  }
1MTD 1HSD { lin-ampl, power-ampl, log-ampl }
1FTD 1HSD { 1HSD HarmonicSpectralDeviation;  }
1MTD 1HOE { lin-ampl, power-ampl, log-ampl }
1FTD 1HOE { 1HOE OddToEvenHarmonicRatio;  }
1MTD 1HTR { lin-ampl, power-ampl, log-ampl }
1FTD 1HTR { 1HTR HarmonicTristimulus;  }
1MTD 1HCN { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1HCN { 1HCN HarmonicSpectralCentroid;  }
1MTD 1HSP { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1HSP { 1HSP HarmonicSpectralSpread;  }
1MTD 1HSK { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1HSK { 1HSK HarmonicSpectralSkewness;  }
1MTD 1HKU { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1HKU { 1HKU HarmonicSpectralKurtosis;  }
1MTD 1HSL { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1HSL { 1HSL HarmonicSpectralSlope;  }
1MTD 1HDE { Value }
1FTD 1HDE { 1HDE HarmonicSpectralDecrease;  }
1MTD 1HRO { Value }
1FTD 1HRO { 1HRO HarmonicSpectralRollOff;  }
1MTD 1HVA { lin-ampl, power-ampl, log-ampl }
1FTD 1HVA { 1HVA HarmonicSpectralVariation;  }

1MTD 1PSD { lin-ampl, power-ampl, log-ampl }
1FTD 1PSD { 1PSD BandSpectralDeviation;  }
1MTD 1POE { lin-ampl, power-ampl, log-ampl }
1FTD 1POE { 1POE OddToEvenBandRatio;  }
1MTD 1PTR { lin-ampl, power-ampl, log-ampl }
1FTD 1PTR { 1PTR BandTristimulus;  }
1MTD 1PCN { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1PCN { 1PCN PerceptualSpectralCentroid;  }
1MTD 1PSP { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1PSP { 1PSP PerceptualSpectralSpread;  }
1MTD 1PSK { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1PSK { 1PSK PerceptualSpectralSkewness;  }
1MTD 1PKU { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1PKU { 1PKU PerceptualSpectralKurtosis;  }
1MTD 1PSL { lin-ampl/lin-freq, power-ampl/lin-freq, log-ampl/lin-freq, lin-ampl/log-freq, power-ampl/log-freq, log-ampl/log-freq }
1FTD 1PSL { 1PSL PerceptualSpectralSlope;  }
1MTD 1PDE { Value }
1FTD 1PDE { 1PDE PerceptualSpectralDecrease;  }
1MTD 1PRO { Value }
1FTD 1PRO { 1PRO PerceptualSpectralRollOff;  }
1MTD 1PVA { lin-ampl, power-ampl, log-ampl }
1FTD 1PVA { 1PVA PerceptualSpectralVariation;  }
1MTD 1LDN { Value }
1FTD 1LDN { 1LDN Loudness;  }
1MTD 1RSL { Value }
1FTD 1RSL { 1RSL RelativeSpecificLoudness;  }
1MTD 1SHA { Value }
1FTD 1SHA { 1SHA Sharpness;  }
1MTD 1SPR { Value }
1FTD 1SPR { 1SPR Spread;  }
1MTD 1SFM { Value }
1FTD 1SFM { 1SFM SpectralFlatness;  }
1MTD 1SCM { Value }
1FTD 1SCM { 1SCM SpectralCrest;  }
1MTD 1MFC { Value }
1FTD 1MFC { 1MFC MFCC;  }
1MTD 1DMF { Value }
1FTD 1DMF { 1DMF DMFCC;  }
1MTD 1DDM { Value }
1FTD 1DDM { 1DDM DDMFCC;  }

1MTD 1ZCR { Value }
1FTD 1ZCR { 1ZCR SignalZeroCrossingRate;  }

1MTD 1HEN { Value }
1FTD 1HEN { 1HEN HarmonicEnergy;  }
1MTD 1NEN { Value }
1FTD 1NEN { 1NEN NoiseEnergy;  }

1MTD 1LAT { Value }
1MTD 1TIN { Value }
1MTD 1TDE { Value }
1MTD 1TCN { Value }
1MTD 1EFD { Value }
1MTD 1BFL { Value }
1MTD 1BRG { Value }
1MTD 1FLS { Value }
1MTD 1RGH { Value }
1FTD 1LTD { 1BEG SegmentStart; 1END SegmentEnd; 1LAT LogAttackTime; 1TIN TemporalIncrease; 1TDE TemporalDecrease; 1TCN TemporalCentroid; 1EFD EffectiveDuration; 1BFL BandFluctuationStrength; 1BRG BandRoughness; 1FLS FluctuationStrength; 1RGH Roughness;  }

1FTD 1WMN { 1BEG SegmentStart; 1END SegmentEnd; 1SCN SpectralCentroid; 1SSP SpectralSpread; 1SSK SpectralSkewness; 1SKU SpectralKurtosis; 1SSL SpectralSlope; 1SDE SpectralDecrease; 1SRO SpectralRollOff; 1SVA SpectralVariation; 1FQ0 FundamentalFrequencyEstimate; 1NSN Noisiness; 1INH Inharmonicity; 1HSD HarmonicSpectralDeviation; 1HOE OddToEvenHarmonicRatio; 1HTR HarmonicTristimulus; 1HCN HarmonicSpectralCentroid; 1HSP HarmonicSpectralSpread; 1HSK HarmonicSpectralSkewness; 1HKU HarmonicSpectralKurtosis; 1HSL HarmonicSpectralSlope; 1HDE HarmonicSpectralDecrease; 1HRO HarmonicSpectralRollOff; 1HVA HarmonicSpectralVariation; 1PSD BandSpectralDeviation; 1POE OddToEvenBandRatio; 1PTR BandTristimulus; 1PCN PerceptualSpectralCentroid; 1PSP PerceptualSpectralSpread; 1PSK PerceptualSpectralSkewness; 1PKU PerceptualSpectralKurtosis; 1PSL PerceptualSpectralSlope; 1PDE PerceptualSpectralDecrease; 1PRO PerceptualSpectralRollOff; 1PVA PerceptualSpectralVariation; 1LDN Loudness; 1RSL RelativeSpecificLoudness; 1SHA Sharpness; 1SPR Spread; 1SFM SpectralFlatness; 1SCM SpectralCrest; 1MFC MFCC; 1DMF DMFCC; 1DDM DDMFCC; 1ARR ARRCoefs; 1ZCR SignalZeroCrossingRate; 1NRG Energy; 1HEN HarmonicEnergy; 1NEN NoiseEnergy;  }

1FTD 1WVR { 1BEG SegmentStart; 1END SegmentEnd; 1SCN SpectralCentroid; 1SSP SpectralSpread; 1SSK SpectralSkewness; 1SKU SpectralKurtosis; 1SSL SpectralSlope; 1SDE SpectralDecrease; 1SRO SpectralRollOff; 1SVA SpectralVariation; 1FQ0 FundamentalFrequencyEstimate; 1NSN Noisiness; 1INH Inharmonicity; 1HSD HarmonicSpectralDeviation; 1HOE OddToEvenHarmonicRatio; 1HTR HarmonicTristimulus; 1HCN HarmonicSpectralCentroid; 1HSP HarmonicSpectralSpread; 1HSK HarmonicSpectralSkewness; 1HKU HarmonicSpectralKurtosis; 1HSL HarmonicSpectralSlope; 1HDE HarmonicSpectralDecrease; 1HRO HarmonicSpectralRollOff; 1HVA HarmonicSpectralVariation; 1PSD BandSpectralDeviation; 1POE OddToEvenBandRatio; 1PTR BandTristimulus; 1PCN PerceptualSpectralCentroid; 1PSP PerceptualSpectralSpread; 1PSK PerceptualSpectralSkewness; 1PKU PerceptualSpectralKurtosis; 1PSL PerceptualSpectralSlope; 1PDE PerceptualSpectralDecrease; 1PRO PerceptualSpectralRollOff; 1PVA PerceptualSpectralVariation; 1LDN Loudness; 1RSL RelativeSpecificLoudness; 1SHA Sharpness; 1SPR Spread; 1SFM SpectralFlatness; 1SCM SpectralCrest; 1MFC MFCC; 1DMF DMFCC; 1DDM DDMFCC; 1ARR ARRCoefs; 1ZCR SignalZeroCrossingRate; 1NRG Energy; 1HEN HarmonicEnergy; 1NEN NoiseEnergy;  }

1FTD 1MDF { 1BEG SegmentStart; 1END SegmentEnd; 1SCN SpectralCentroid; 1SSP SpectralSpread; 1SSK SpectralSkewness; 1SKU SpectralKurtosis; 1SSL SpectralSlope; 1SDE SpectralDecrease; 1SRO SpectralRollOff; 1SVA SpectralVariation; 1FQ0 FundamentalFrequencyEstimate; 1NSN Noisiness; 1INH Inharmonicity; 1HSD HarmonicSpectralDeviation; 1HOE OddToEvenHarmonicRatio; 1HTR HarmonicTristimulus; 1HCN HarmonicSpectralCentroid; 1HSP HarmonicSpectralSpread; 1HSK HarmonicSpectralSkewness; 1HKU HarmonicSpectralKurtosis; 1HSL HarmonicSpectralSlope; 1HDE HarmonicSpectralDecrease; 1HRO HarmonicSpectralRollOff; 1HVA HarmonicSpectralVariation; 1PSD BandSpectralDeviation; 1POE OddToEvenBandRatio; 1PTR BandTristimulus; 1PCN PerceptualSpectralCentroid; 1PSP PerceptualSpectralSpread; 1PSK PerceptualSpectralSkewness; 1PKU PerceptualSpectralKurtosis; 1PSL PerceptualSpectralSlope; 1PDE PerceptualSpectralDecrease; 1PRO PerceptualSpectralRollOff; 1PVA PerceptualSpectralVariation; 1LDN Loudness; 1RSL RelativeSpecificLoudness; 1SHA Sharpness; 1SPR Spread; 1SFM SpectralFlatness; 1SCM SpectralCrest; 1MFC MFCC; 1DMF DMFCC; 1DDM DDMFCC; 1ARR ARRCoefs; 1ZCR SignalZeroCrossingRate; 1NRG Energy; 1HEN HarmonicEnergy; 1NEN NoiseEnergy;  }

1FTD 1MDA { 1BEG SegmentStart; 1END SegmentEnd; 1SCN SpectralCentroid; 1SSP SpectralSpread; 1SSK SpectralSkewness; 1SKU SpectralKurtosis; 1SSL SpectralSlope; 1SDE SpectralDecrease; 1SRO SpectralRollOff; 1SVA SpectralVariation; 1FQ0 FundamentalFrequencyEstimate; 1NSN Noisiness; 1INH Inharmonicity; 1HSD HarmonicSpectralDeviation; 1HOE OddToEvenHarmonicRatio; 1HTR HarmonicTristimulus; 1HCN HarmonicSpectralCentroid; 1HSP HarmonicSpectralSpread; 1HSK HarmonicSpectralSkewness; 1HKU HarmonicSpectralKurtosis; 1HSL HarmonicSpectralSlope; 1HDE HarmonicSpectralDecrease; 1HRO HarmonicSpectralRollOff; 1HVA HarmonicSpectralVariation; 1PSD BandSpectralDeviation; 1POE OddToEvenBandRatio; 1PTR BandTristimulus; 1PCN PerceptualSpectralCentroid; 1PSP PerceptualSpectralSpread; 1PSK PerceptualSpectralSkewness; 1PKU PerceptualSpectralKurtosis; 1PSL PerceptualSpectralSlope; 1PDE PerceptualSpectralDecrease; 1PRO PerceptualSpectralRollOff; 1PVA PerceptualSpectralVariation; 1LDN Loudness; 1RSL RelativeSpecificLoudness; 1SHA Sharpness; 1SPR Spread; 1SFM SpectralFlatness; 1SCM SpectralCrest; 1MFC MFCC; 1DMF DMFCC; 1DDM DDMFCC; 1ARR ARRCoefs; 1ZCR SignalZeroCrossingRate; 1NRG Energy; 1HEN HarmonicEnergy; 1NEN NoiseEnergy;  }

1FTD 1WSD { 1BEG SegmentStart; 1END SegmentEnd; 1SCN SpectralCentroid; 1SSP SpectralSpread; 1SSK SpectralSkewness; 1SKU SpectralKurtosis; 1SSL SpectralSlope; 1SDE SpectralDecrease; 1SRO SpectralRollOff; 1SVA SpectralVariation; 1FQ0 FundamentalFrequencyEstimate; 1NSN Noisiness; 1INH Inharmonicity; 1HSD HarmonicSpectralDeviation; 1HOE OddToEvenHarmonicRatio; 1HTR HarmonicTristimulus; 1HCN HarmonicSpectralCentroid; 1HSP HarmonicSpectralSpread; 1HSK HarmonicSpectralSkewness; 1HKU HarmonicSpectralKurtosis; 1HSL HarmonicSpectralSlope; 1HDE HarmonicSpectralDecrease; 1HRO HarmonicSpectralRollOff; 1HVA HarmonicSpectralVariation; 1PSD BandSpectralDeviation; 1POE OddToEvenBandRatio; 1PTR BandTristimulus; 1PCN PerceptualSpectralCentroid; 1PSP PerceptualSpectralSpread; 1PSK PerceptualSpectralSkewness; 1PKU PerceptualSpectralKurtosis; 1PSL PerceptualSpectralSlope; 1PDE PerceptualSpectralDecrease; 1PRO PerceptualSpectralRollOff; 1PVA PerceptualSpectralVariation; 1LDN Loudness; 1RSL RelativeSpecificLoudness; 1SHA Sharpness; 1SPR Spread; 1SFM SpectralFlatness; 1SCM SpectralCrest; 1MFC MFCC; 1DMF DMFCC; 1DDM DDMFCC; 1ARR ARRCoefs; 1ZCR SignalZeroCrossingRate; 1NRG Energy; 1HEN HarmonicEnergy; 1NEN NoiseEnergy;  }
}

") 