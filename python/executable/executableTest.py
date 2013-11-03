import os

dictPath = '~/Research/OM-pursuit/dictionaries/strings+benvibes.dict.sdif'
constraintPath = '/Users/geb/Research/OM-Pursuit/constraints/harm_fof/harm_fof-2.mpctr.sdif'
targetPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.wav'
markerPath = '~/Research/OM-Pursuit/target/harm_fof/harm_fof.mrk.sdif'
mpconstrPath = '~/Research/OM-Pursuit/constraints/new-mp-constraints/maxatoms-and-corpatoms-and-mindistance.glmpctr.sdif'

root = '~/Research/OM-Pursuit/output'
modPath = '%s/modEx.wav'%root
resPath = '%s/resEx.wav'%root
sdifPath = '%s/bookEx.sdif'%root

maxiter = 10

os.chdir('./dist/OM-Pursuit.app/Contents/MacOS')
os.system('./OM-Pursuit %s %s %s %s %s %s %s %s %i --dsf 2.0'%(dictPath, constraintPath, targetPath, markerPath, mpconstrPath, modPath, resPath, sdifPath, maxiter))
os.chdir('../../../..')
