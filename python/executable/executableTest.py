import os

dictPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/dictionaries/Benvibes/Benvibes-new.dict.sdif'
constraintPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/constraints/newbuzz'
targetPath = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/targets/buzz/buzz-scale.aiff'
markerPath = '"/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/targets/buzz/buzz-scale_markers(50ms).sdif"'
mpconstrPath = 'Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/constraints/newbuzz/buzz-1FQ0-2ndorder.sdif'

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
