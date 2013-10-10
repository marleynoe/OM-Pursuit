import OMPursuit
import os 

d = './test_sdifs/constraint-SDIFs'

for f in os.listdir(d):
    if os.path.splitext(f)[1].lower() == '.sdif':
        C = OMPursuit.OMPursuitCompoundConstraint(d + '/' + f)
        print('################################################\n')
        
