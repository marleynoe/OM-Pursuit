import OMPursuit
import numpy as np
import matplotlib.pyplot as plt

#path = './test_sdifs/testSDIF5.sdif'
path = './test/Benvibes-test-inclmidi.dict.sdif'
constraint_path = './test_sdifs/constraint-SDIFs/test-constraints(single-conditional).pct.sdif'

D = OMPursuit.OMPursuitDictionary(path, 1)
C = OMPursuit.OMPursuitCompoundConstraint(constraint_path)

print(C.cFullConstraintFunction(D, 45.0))

