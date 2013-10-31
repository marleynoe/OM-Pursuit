import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt
import scikits.audiolab as audiolab

dsfactor = 4
maxit = 1000
mindist = 0.0
maxsimul = 1000

paths = ['/Users/geb/Research/OM-Pursuit/dictionaries/Benvibes-inclmidi.dict.sdif']
constraintPaths = ['/Users/geb/Research/OM-Pursuit/constraints/harm_fof-symbolic']
targetPaths = ['/Users/geb/Research/OM-Pursuit/target/harm_fof/harm_fof.wav']
markerPaths = ['/Users/geb/Research/OM-Pursuit/target/harm_fof/harm_fof.mrk.sdif']

for i, path in enumerate(paths):

    outpath = '~/Research/Om-Pursuit/output/' + os.path.splitext(os.path.basename(path))[0] + '-' + os.path.splitext(os.path.basename(markerPaths[i]))[0] + '-' + os.path.splitext(os.path.basename(constraintPaths[i]))[0] + '-' + os.path.splitext(os.path.basename(targetPaths[i]))[0] 

    D = OMPursuit.OMPursuitDictionary(path, dsfactor)
    markers = OMPursuit.OMPursuitMarkers(markerPaths[i])

    for ci, constraint in enumerate(os.listdir(os.path.expanduser(constraintPaths[i]))):
        if os.path.splitext(constraint)[1].lower() == '.sdif':

            print(constraint)

            C = OMPursuit.OMPursuitCompoundConstraint(constraintPaths[i] + '/' + constraint)
            target = OMPursuit.OMPursuitTarget(targetPaths[i], dsfactor)
            target.zeropadSignal(D, markers)

            A = OMPursuit.OMPursuitAnalysis(D, C, target, markers, maxit, mindist, maxsimul)
            A.constrainedMP()
            audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser(outpath + '-%d.aif'%ci), A.ompModel.samplerate)

            #log the analysis data
            f = open(os.path.expanduser(outpath + '-%d.txt'%ci), 'w')
            f.write('Dictionary : %s\n'%path)
            f.write('Constraint : %s\n'%constraint)
            f.write('Markers : %s\n'%markerPaths[i])
            f.write('Max. iterations : %d\n'%maxit)
            f.write('Min. dist (s) %0.4f\n'%mindist)
            f.write('Max. simul. soundgrains : %d\n'%maxsimul)
            f.write('Total soundgrains in model : %d'%len(A.ompModel.parameterArray))
            f.close()

            A.writeModelSdif(outpath + '-%d.sdif'%ci)
