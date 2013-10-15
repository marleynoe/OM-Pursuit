import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt
import scikits.audiolab as audiolab

path = '~/Research/OM-Pursuit/sounds/test/Benvibes-test-inclmidi.dict.sdif'
constraint_path = '~/Research/OM-Pursuit/constraints'
targetPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.wav' 
markerPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof-mixed-mrk.sdif' 
dsfactor = 1

D = OMPursuit.OMPursuitDictionary(path, dsfactor)
markers = OMPursuit.OMPursuitMarkers(markerPath)

for ci, constraint in enumerate(os.listdir(os.path.expanduser(constraint_path))):
    if os.path.splitext(constraint)[1].lower() == '.sdif':
        C = OMPursuit.OMPursuitCompoundConstraint(constraint_path + '/' + constraint)
        '''
        for n in markers.times:
            s = C.cFullConstraintFunction(D, n)
            for key, value in D.soundgrains.iteritems():
                print(key, value.tempGain)
            D.reinitWeights()
        '''
        target = OMPursuit.OMPursuitTarget(targetPath, dsfactor)
        target.zeropadSignal(D, markers)

        #omdictionary, omcompoundconstraint, omtarget, ommarkers, maxtotal, mindistance, maxsimultaneous)
        A = OMPursuit.OMPursuitAnalysis(D, C, target, markers, 1000, 0.05, 2)
        A.constrainedMP()
        audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser('~/Research/OM-Pursuit/output/harm_fof-with-constraint%d.aif'%ci), A.ompModel.samplerate)
        #print(A.ompModel.parameterArray)
