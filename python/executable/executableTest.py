import os

dictPath = '~/Research/OM-pursuit/dictionaries/strings+benvibes.dict.sdif'
constraintPath = '~/Research/OM-Pursuit/constraints/harm_fof/harm_fof-2.mpctr.sdif'
targetPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.wav'
markerPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.mrk.sdif'
mpconstrPath = '~/Research/OM-Pursuit/constraints/new-mp-constraints/maxatoms-and-corpatoms-and-mindistance.glmpctr.sdif'

root = '~/Research/OM-Pursuit/output'
modPath = '%s/modEx.aif'%root
resPath = '%s/resEx.aif'%root
sdifPath = '%s/bookEx.sdif'%root
logFile = '~/Desktop/logFile.txt'

maxiter = 10
dsfactor = 1

os.chdir('./dist/OM-Pursuit.app/Contents/MacOS')
#os.system('./OM-Pursuit %s %s %s %s %s %s %i --constraint_path %s --mpconstraint_path %s --dsf %i'%(dictPath, targetPath, markerPath, modPath, resPath, sdifPath, maxiter, constraintPath, mpconstrPath, dsfactor))
os.system('./OM-Pursuit %s %s %s %s %s %s %i --logfile %s'%(dictPath, targetPath, markerPath, modPath, resPath, sdifPath, maxiter, logFile))
os.chdir('../../../..')
