import OMPursuit
import os 

d = '/Users/geb/Documents/Development/OM-Pursuit_final/OM-Pursuit_final/constraints/buzz'

for f in os.listdir(d):
    if os.path.splitext(f)[1].lower() == '.sdif':
    	print(f)
        C = OMPursuit.OMPursuitCompoundConstraint(d + '/' + f)
        print('################################################\n')
        
