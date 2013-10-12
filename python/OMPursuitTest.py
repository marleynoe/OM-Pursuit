import os
import OMPursuit
import numpy as np
import matplotlib.pyplot as plt

path = '~/Research/OM-Pursuit/sounds/test/Benvibes-test-inclmidi.dict.sdif'
constraint_path = '~/Research/OM-Pursuit/constraints'

D = OMPursuit.OMPursuitDictionary(path, 1)
for constraint in os.listdir(os.path.expanduser(constraint_path)):
    if os.path.splitext(constraint)[1].lower() == '.sdif':
        print(constraint)
        C = OMPursuit.OMPursuitCompoundConstraint(constraint_path + '/' + constraint)
        for n in np.linspace(0., 2., 100):
            s = C.cFullConstraintFunction(D, n)
            print(s)
            for key, value in D.soundgrains.iteritems():
                print(key, value.tempGain)
            D.reinitWeights()
    

