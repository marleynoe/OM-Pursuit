import scipy.signal as sig
import numpy as np
import time

N = 10 * 44100
k = 1000
i = 0
factor = 8

x = np.random.randn(N)
p = time.clock()

while i < k:
    sig.resample(x, N / float(factor))
    #sig.decimate(x, factor)
    i += 1

print(time.clock() - p)
    
