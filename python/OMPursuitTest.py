import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt
import scikits.audiolab as audiolab

#path = '~/Research/OM-Pursuit/dictionaries/Benvibes-inclmidi.dict.sdif'
#path = '~/Research/OM-pursuit/dictionaries/Percussion.dict.sdif'
#path = '~/Research/OM-pursuit/dictionaries/Breakbeats-separate.dict.sdif'
#path = '~/Research/OM-pursuit/dictionaries/Breakbeats.dict.sdif'
#path = '~/Research/OM-pursuit/dictionaries/strings.dict.sdif'
path = '~/Research/OM-pursuit/dictionaries/strings+benvibes.dict.sdif'

constraint_path = '/Users/geb/Research/OM-Pursuit/constraints/harm_fof'

targetPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.wav'
#targetPath = '~/Research/OM-Pursuit/target/drumloop/drumLoop_mono.aif' 
#targetPath = '~/Research/OM-Pursuit/target/harley/Harley2.aif'
#targetPath = '~/Research/OM-Pursuit/target/fof-synth/FOF-3/FOF-synth-3.aiff'

markerPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.mrk.sdif' 
#markerPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.mrk.sdif'
#markerPath = '~/Research/OM-Pursuit/target/drumloop/drumLoop_mono.mrk.sdif'
#markerPath = '~/Research/OM-Pursuit/target/harley/Harley2-mrk.sdif'
#markerPath = '~/Research/OM-Pursuit/target/fof-synth/FOF-3/fof-synth-3-gauss.mrk.sdif'

mpconstrPath = '/Users/geb/Research/OM-Pursuit/constraints/new-mp-constraints/maxatoms-and-corpatoms-and-mindistance.glmpctr.sdif'

dsfactor = 1
maxit = 50

for ci, constraint in enumerate(os.listdir(os.path.expanduser(constraint_path))):

    if os.path.splitext(constraint)[1].lower() == '.sdif':

        print(constraint)
        
        D = OMPursuit.OMPursuitDictionary(path, dsfactor)
        Ac = OMPursuit.OMPAnalysisConstraintSet(mpconstrPath)
        C = OMPursuit.OMPursuitCompoundConstraint(constraint_path + '/' + constraint)
        markers = OMPursuit.OMPursuitMarkers(markerPath)
        target = OMPursuit.OMPursuitTarget(targetPath, dsfactor)
        target.zeropadSignal(D, markers)

        #omdictionary, omcompoundconstraint, omtarget, ommarkers, maxtotal, mindistance, maxsimultaneous)
        A = OMPursuit.OMPursuitAnalysis(D, C, target, markers, Ac, maxit)
        A.constrainedMP()
        audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser('~/Research/OM-Pursuit/output/FOF3%d.aif'%ci), A.ompModel.samplerate)

        f = open(os.path.expanduser('~/Research/OM-Pursuit/output/FOF3%d.txt'%ci), 'w')
        f.write('Dictionary : %s\n'%path)
        f.write('Constraint : %s\n'%constraint)
        f.write('Markers : %s\n'%markerPath)
        f.write('Max. iterations : %d\n'%maxit)
        f.write('Total soundgrains in model : %d'%len(A.ompModel.parameterArray))
        f.close()

        A.writeModelSdif('~/Research/OM-Pursuit/output/FOF3%d.sdif'%ci)


        #print(A.ompModel.parameterArray)
