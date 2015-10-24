'''
    OMPursuitTest.py
    Created by Graham Boyes 01-10-2013
    '''

import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt
import scikits.audiolab as audiolab
import time

path = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/dictionaries/Benvibes/Benvibes-new.dict.sdif'
constraint_path = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/constraints/newbuzz'
# constraint_path = '/Users/geb/Desktop/notworking'
targetPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/targets/buzz/buzz-scale.aiff'
markerPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/targets/buzz/buzz-scale_markers(50ms).sdif'
mpconstrPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/constraints/mp-constraints/buzz_maxatoms+min_inter_onset.mpctr.sdif'

dsfactor = 1
maxit = 100

for ci, constraint in enumerate(os.listdir(os.path.expanduser(constraint_path))):

    if os.path.splitext(constraint)[1].lower() == '.sdif':

        print(constraint)
        
        p = time.clock()
        D = OMPursuit.OMPursuitDictionary(path, dsfactor)

        print("Total time is %0.1f"%(time.clock()-p))

        Ac = OMPursuit.OMPAnalysisConstraintSet(mpconstrPath)

        C = OMPursuit.OMPursuitCompoundConstraint(constraint_path + '/' + constraint)

        markers = OMPursuit.OMPursuitMarkers(markerPath)
        target = OMPursuit.OMPursuitTarget(targetPath, dsfactor)
        target.zeropadSignal(D, markers)

        #omdictionary, omcompoundconstraint, omtarget, ommarkers, maxtotal, mindistance, maxsimultaneous)
        A = OMPursuit.OMPursuitAnalysis(D, target, markers, maxit, constraint=C, mpconstraint=Ac)
        #A = OMPursuit.OMPursuitAnalysis(D, target, markers, maxit, constraint=None, mpconstraint=None)
        A.constrainedMP()

        audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser('~/Research/OM-Pursuit/output/buzz%d.aif'%ci), A.ompModel.samplerate)
 
        f = open(os.path.expanduser('~/Research/OM-Pursuit/output/buzz%d.txt'%ci), 'w')
        f.write('Dictionary : %s\n'%path)
        f.write('Constraint : %s\n'%constraint)
        f.write('Markers : %s\n'%markerPath)
        f.write('Max. iterations : %d\n'%maxit)
        f.write('Total soundgrains in model : %d'%len(A.ompModel.parameterArray))
        f.close()

        A.writeModelSdif('~/Research/OM-Pursuit/output/buzz%d.sdif'%ci)
        
