'''
    OMPursuitTestScript.py
    Created by Graham Boyes 01-10-2013
    '''
import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt
import scikits.audiolab as audiolab

dsfactor = 1
maxit = 1000

paths = ['/Users/geb/Research/OM-Pursuit/dictionaries/Percussion.dict.sdif']
constraintPaths = ['/Users/geb/Research/OM-Pursuit/constraints/rally/rally-35kSSP-or-40SCN.ctr.sdif', '/Users/geb/Research/OM-Pursuit/constraints/rally/rally-35kSSP-wSSK-and-40SCN-wNEN.ctr.sdif', '/Users/geb/Research/OM-Pursuit/constraints/rally/rally-35kSSP-wSSK-or-40SCN-wNEN.ctr.sdif', '/Users/geb/Research/OM-Pursuit/constraints/rally/rally-50SSP-or-50SCN.ctr.sdif', '/Users/geb/Research/OM-Pursuit/constraints/rally/rally-wSSK-and-wNEN.ctr.sdif', '/Users/geb/Research/OM-Pursuit/constraints/rally/rally-wSSK-and-wNEN.ctr1.sdif']
targetPaths = ['/Users/geb/Research/OM-Pursuit/target/rally/Rally.aif' for i in range(6)]
markerPaths = ['/Users/geb/Research/OM-Pursuit/target/rally/Rally-mrk.sdif' for i in range(6)]

mpconstrPath = '/Users/geb/Research/OM-Pursuit/constraints/rally-voix_mp-constraints/rally-voice-50-5simul.mpctr.sdif'

for i, path in enumerate(paths):

    outpath = '~/Research/Om-Pursuit/output/' + os.path.splitext(os.path.basename(path))[0] + '-' + os.path.splitext(os.path.basename(markerPaths[i]))[0] + '-' + os.path.splitext(os.path.basename(constraintPaths[i]))[0] + '-' + os.path.splitext(os.path.basename(targetPaths[i]))[0] 

    #for ci, constraint in enumerate(os.listdir(os.path.expanduser(constraintPaths[i]))):
    for ci, constraint in enumerate(constraintPaths):
        if os.path.splitext(constraint)[1].lower() == '.sdif':

            print(constraint)

            D = OMPursuit.OMPursuitDictionary(path, dsfactor)
            markers = OMPursuit.OMPursuitMarkers(markerPaths[i])
            Ac = OMPursuit.OMPAnalysisConstraintSet(mpconstrPath)

            C = OMPursuit.OMPursuitCompoundConstraint(constraint)
            target = OMPursuit.OMPursuitTarget(targetPaths[i], dsfactor)
            target.zeropadSignal(D, markers)

            A = OMPursuit.OMPursuitAnalysis(D, target, markers, maxit, constraint=C, mpconstraint=Ac)
            A.constrainedMP()
            audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser(outpath + '-%d.aif'%ci), A.ompModel.samplerate)

            #log the analysis data
            f = open(os.path.expanduser(outpath + '-%d.txt'%ci), 'w')
            f.write('Dictionary : %s\n'%path)
            f.write('Constraint : %s\n'%constraint)
            f.write('MPConstraint : %s\n'%mpconstrPath)
            f.write('Markers : %s\n'%markerPaths[i])
            f.write('Max. iterations : %d\n'%maxit)
            f.write('Total soundgrains in model : %d'%len(A.ompModel.parameterArray))
            f.close()

            A.writeModelSdif(outpath + '-%d.sdif'%ci)
