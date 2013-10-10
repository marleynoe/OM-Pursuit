import numpy as np
import scipy.signal as sig
import scipy.linalg as linalg
import scikits.audiolab as audiolab
import pysdif
import os
import re
import operator
from scipy.interpolate import interp1d

class OMPursuitConstraint:
    def __init__(self, constraintString):

        #Will use this later to build the expression from the fullconstraint
        constraintString = constraintString[1:]
        self.cString = constraintString
        self.cString = '(' + self.cString + ')'
        print(self.cString)

        operatorLookup = {'<' : operator.lt, '<=' : operator.le, '=' : operator.eq, '!=' : operator.ne, '>=' : operator.ge, '>' : operator.gt}

        parsedConstraintString = re.split(' ', constraintString)
        print(parsedConstraintString)
        self.cSignature = parsedConstraintString[1]
        print(self.cSignature)

        if parsedConstraintString[0] in operatorLookup.keys():
            self.cType = 'conditional'
            self.operator = operatorLookup[parsedConstraintString[0]]
            print(self.operator)

        elif parsedConstraintString[0].lower() == 'w':
            self.cType = 'weight'

        elif parsedConstraintString[0].lower() == 'b':
            self.cType = 'bias'
                
        elif parsedConstraintString[0].isdigit():
            self.cType = 'k-nearest'
            self.kn = eval(parsedConstraintString[0])
            print(self.kn)
        
        self.index = eval(parsedConstraintString[3])
        print(self.index)
        print(self.cType)

    def interpolatedValue(self, time, kind='linear'):
        f = interp1d(self.cTimes, self.cValues, kind=kind)
        return f(time)

    def cFunction(self, D, time, indices=None):
        value = self.interpolatedValue(time)

        if self.cType == 'conditional':
            return set([i for i in range(1, D.len()+1) if self.operator(D.soundgrains[i].averagedDescriptors[self.cSignature], value)])

        elif self.cType == 'weight':

            #find indices
            if not indices:
                indices = set([i for i in range(1, D.len()+1)])

            #normalize according to signature
            q = np.zeros(D.len())

            for k in indices:
                q[k-1] = D.soundgrains[k].averagedDescriptors[self.cSignature].copy()
            m = max(abs(q))
            q *= 1.0/m

            #apply weights
            for k in indices:
                D.constraints[k].tempGain *= q[k-1] 
     
            return indices

        elif self.cType == 'bias':

            if not indices:
                indices = set([i for i in range(1, D.len()+1)])
            for k in indices:
                D.constraints[k].tempGain *= value

            return indices

        elif self.cType == 'k-nearest':
       
            #can always work with all the atoms
            q = np.zeros(D.len())
            for k in range(1, D.len()+1):
                q[k] = D.soundgrains[k].averagedDescriptors[self.cSignature].copy()
            
            return set(np.argsort(np.abs(q - value))[0:self.kn] + 1)



class OMPursuitCompoundConstraint:
    '''Parse a constraint definition sdif and serve as a container for OMPusuitConstraint instances'''

    def __init__(self, constraintSdifPath):

        sdifFile = pysdif.SdifFile(constraintSdifPath, 'r')
        sc = sdifFile.get_stream_IDs()

        self.constraints = {}
        self.operatorLookup = {'and' : operator.and_, 'or' : operator.or_ , 'xor' : operator.xor, 'nand' : lambda p, q, r : operator.xor(p, (q & r)), 'nor' : lambda p, q, r : operator.xor(p, (q|r))}

        self.numConstraints = len(sc)

        constraintValues = [[] for i in range(self.numConstraints)]
        constraintTimes = [[] for i in range(self.numConstraints)]        

        #obtain the full constraint expression
        for nvt in sdifFile.get_NVTs():
            if 'Fullconstraint' in nvt.keys():
                self.fullConstraintString = nvt['Fullconstraint']
                break

        print(self.fullConstraintString)
        
        #initialize the OMPursuiConstraintObjects
        for i, constraint in enumerate(sc):
            C = OMPursuitConstraint(constraint.treeway)
            self.constraints[i+1] = C

        #read the frames from the sdif file
        for frame in sdifFile:
            constraintTimes[i-1].append(frame.time)
            for matrix in frame:  
                constraintValues[i-1].append(matrix.get_data()[0][0])

        #add the times and values to the constraint 
        for i in range(1, self.numConstraints+1):
            self.constraints[i].cValues = np.array(constraintValues[i-1])
            self.constraints[i].cTimes = np.array(constraintTimes[i-1])
        

        for c in self.constraints.values():
            self.fullConstraintString = re.sub(c.cString, str(c.index), self.fullConstraintString)
        print(self.fullConstraintString)


    def cFullConstraintFunction(self, D, time):
        #simplest case, only one constraint
        if self.fullConstraintString == '(1)':
            return self.constraints[1].cFunction(D, time)    

         
    

class OMPursuitSoundgrain:
    '''Data representation for a single sound file within an OMPursuitDictionary'''

    def __init__(self, soundfilePath, downsampleFactor):

        sformats = {'.aif' : audiolab.aiffread, '.aiff' : audiolab.aiffread, '.wav' : audiolab.wavread}
        readFunc = sformats[(os.path.splitext(soundfilePath)[1]).lower()]
        x, fs, p = readFunc(soundfilePath)

        if len(np.shape(x)) > 1:
            x = x[:, 0]

        self.norm = linalg.norm(x)#the original norm, this is needed to do the rescaling later in OM
        self.samplerate = fs            

        if downsampleFactor > 1:
            x = sig.resample(x, len(x)/downsampleFactor)
            self.samplerate = fs / downsampleFactor
        elif downsampleFactor < 1:
            raise Exception("Upsampling not permitted")

        self._signal = x / linalg.norm(x) #the normalization step occurs once'''
        self.tempGain = 1.0 #will use this later for the weighting, with this implementation analyses for time points must be done sequentially 

    @property
    def signal(self):
        '''getter for signal that implicitly applies the associated gain'''
        return self._signal * self.tempGain


class OMPursuitDictionary:
    '''Parse an SDIF file and act as a container for OMPursuitSoundgrain instances'''    

    def __init__(self, sdifPath, downsampleFactor):

        #obtain the sdif file object
        sdifFile = pysdif.SdifFile(sdifPath, 'r')
        
        #obtain the references to the streams in sdifFile
        streamRefs = sdifFile.get_stream_IDs()

        #the number of sound references  
        self.numRefs = len(streamRefs) 
        
        #assign a lookup for soundgrains
        self.soundgrains = {}
        for i, sg in enumerate(streamRefs):
            self.soundgrains[i+1] = OMPursuitSoundgrain(sg.source, downsampleFactor)
            
        #build the descriptor object (holds the raw data, might not need this at a later stage...)
        self.descriptorLookup = {}
        for i in range(1, self.numRefs+1):
            self.descriptorLookup[i] = {}
            for ft in sdifFile.get_frame_types():
                self.descriptorLookup[i][ft.signature]  = {}
                for component in ft.components:
                    self.descriptorLookup[i][ft.signature][component.signature] = {}
                    self.descriptorLookup[i][ft.signature][component.signature]['name'] = component.name
                    self.descriptorLookup[i][ft.signature][component.signature]['num'] = component.num
                    self.descriptorLookup[i][ft.signature][component.signature]['values'] = []
                    self.descriptorLookup[i][ft.signature][component.signature]['time'] = []

        #read the descriptor values from the sdif and fill the lookup table
        for frame in sdifFile:
            for matrix in frame:
                self.descriptorLookup[frame.id][frame.signature][matrix.signature]['values'].append((matrix.get_data()[0]))
                self.descriptorLookup[frame.id][frame.signature][matrix.signature]['time'].append(frame.time) 

        #get the dimension in order to build the ndarrays
        maxy = [0, 0]
        
        for i, matsig in enumerate(['1DSC', '1WMN']):
            for k in range(1, self.numRefs+1):  
                for key in self.descriptorLookup[k][matsig].keys():
                    n = len(self.descriptorLookup[k][matsig][key]['time'])
                    if n > maxy[i]:
                        maxy[i] = n

        dtype = [(d, float) for d in self.descriptorLookup[1]['1WMN'].keys()]
        dtype.append(('time', float))

        for i in range(1, self.numRefs+1):
            for q, ys in enumerate(maxy):
                if q == 0:
                    self.soundgrains[i].shortTimeDescriptors = np.zeros(ys, dtype=dtype)
                    dummyArray = self.soundgrains[i].shortTimeDescriptors 
                else:
                    self.soundgrains[i].averagedDescriptors = np.zeros(ys, dtype=dtype)
                    dummyArray = self.soundgrains[i].averagedDescriptors
 
                for j in range(maxy[1]):
                    checkTime = True
                    for key in [d[0] for d in dtype]:
                        if key != 'time':
                            if len(self.descriptorLookup[i]['1WMN'][key]['values']) > j:
                                dummyArray[j][key] =  self.descriptorLookup[i]['1WMN'][key]['values'][j]
                                if checkTime:   
                                    checkTime = False
                                    dummyArray[j]['time'] = self.descriptorLookup[i]['1WMN'][key]['time'][j]

        self.indices = set(self.soundgrains.keys()) #store the set of indices to apply the constriant operations later

    def len(self):
        return len(self.soundgrains.keys())

    def reinitWeights(self):
        for i in self.soundgrains.keys():
            self.soundgrains[i].tempGain = 1.0    
