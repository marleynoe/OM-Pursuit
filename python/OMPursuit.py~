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

        else:
            raise Exception        

        self.index = eval(parsedConstraintString[3])
        print(self.index)
        print(self.cType)

    def interpolatedValue(self, time, kind='linear'):
        f = interp1d(self.cTimes, self.cValues, kind=kind)
        try:
            x = f(time)
        #if the time is outside of the range of the function, return the last value
        except ValueError:
            if time > self.cTimes[len(self.cValues)-1]:
                x = self.cValues[len(self.cValues)-1]
            elif time < self.cTimes[0]:
                x = self.cValues[0]
            else:
                raise Exception
        return x

    def cFunction(self, D, time):
        value = self.interpolatedValue(time)

        if self.cType == 'conditional':
            return set([i for i in range(1, D.len()+1) if self.operator(D.soundgrains[i].averagedDescriptors[self.cSignature], value)])

        elif self.cType == 'weight':

            indices = [i for i in range(1, D.len()+1)]

            #normalize according to signature
            q = np.zeros(D.len())

            for k in indices:
                q[k-1] = D.soundgrains[k].averagedDescriptors[self.cSignature].copy()

            m = max(abs(q))
            q *= 1.0/m

            return dict(zip(indices, q))

        elif self.cType == 'bias':
            return value

        elif self.cType == 'k-nearest':
       
            #can always work with all the atoms
            q = np.zeros(D.len())
            for k in range(1, D.len()+1):
                q[k-1] = D.soundgrains[k].averagedDescriptors[self.cSignature].copy()
            
            return set(np.argsort(np.abs(q - value))[0:self.kn] + 1)


class OMPursuitCompoundConstraint:
    '''Parse a constraint definition sdif and serve as a container for OMPusuitConstraint instances'''

    def __init__(self, constraintSdifPath):

        sdifFile = pysdif.SdifFile(os.path.expanduser(constraintSdifPath), 'r')
        sc = sdifFile.get_stream_IDs()

        self.constraints = {}
        self.operatorLookup = {'and ' : 'self.andOMP', 'or ' : 'self.orOMP', 'xor ': 'self.xorOMP', 'nand ' : 'self.nandOMP', 'nor ' : 'self.norOMP', 'xnor ' : 'self.xnorOMP', 'b ' : 'self.biasOMP', 'w ' : 'self.weightOMP'}
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
            constraintTimes[frame.id-1].append(frame.time)
            for matrix in frame:  
                constraintValues[frame.id-1].append(matrix.get_data()[0][0])

        #add the times and values to the constraint 
        for i in range(1, self.numConstraints+1):
            self.constraints[i].cValues = np.array(constraintValues[i-1])
            self.constraints[i].cTimes = np.array(constraintTimes[i-1])

    def cFullConstraintFunction(self, D, time):

        if not hasattr(self, 'dictionaryReference'):        
            self.dictionaryReference = D #keep a reference to the dictionary in name space
            self.fullConstraintString = re.sub('nil', str(set(D.soundgrains.keys())), self.fullConstraintString)

        #simplest case, only one constraint
        tempString = self.fullConstraintString

        #first check and replace the constraint functions
        for c in self.constraints.values():
            tempString = re.sub(c.cString, str(c.cFunction(D, time)), tempString)

        #then check and replace the combination operators        
        for k in self.operatorLookup.keys():
            if k in tempString: 
                tempString = re.sub(k, self.operatorLookup[k], tempString)
        
        #finally return the evaluated string, yields a set of indices
        return(eval(tempString))


    #TODO: refactor, many of these are duplicates of functions and are added for consistency 
    def andOMP(self, indices1, indices2):
        return operator.and_(indices1, indices2)

    def orOMP(self, indices1, indices2):
        return operator.or_(indices1, indices2)

    def xorOMP(self, indices1, indices2):
        return operator.xor_(indices1, indices2)

    def nandOMP(self, indices1, indices2):
        if hasattr(self, 'dictionaryReference'):
            return operator.xor(set(self.dictionaryReference.keys()), operator.and_(indices1, indices2))
        else:
            return set([]) 

    def norOMP(self, indices1, indices2):
        if hasattr(self, 'dictionaryReference'):
            return operator.xor(set(self.dictionaryReference.keys()), operator.or_(indices1, indices2))
        else:
            return set([])

    def xnorOMP(self, indices1, indices2):
        if hasattr(self, 'dictionaryReference'):
            return operator.xor(set(self.dictionaryReference.keys()), operator.xor(indices1, indices2))
        else:
            return set([])    

    def biasOMP(self, indices, bvalue):
         if hasattr(self, 'dictionaryReference'):
             for k in indices:
                 self.dictionaryReference.soundgrains[k].tempGain *= bvalue
         return indices #just return the input indices

    def weightOMP(self, indices, wdict):
        if hasattr(self, 'dictionaryReference'):
            for k in indices:
                self.dictionaryReference.soundgrains[k].tempGain *= wdict[k]
        return indices #just return the input indices


class OMPursuitSoundgrain:
    '''Data representation for a single sound file within an OMPursuitDictionary'''

    def __init__(self, soundfilePath, downsampleFactor):

        sformats = {'.aif' : audiolab.aiffread, '.aiff' : audiolab.aiffread, '.wav' : audiolab.wavread}
        readFunc = sformats[(os.path.splitext(soundfilePath)[1]).lower()]
        x, fs, p = readFunc(os.path.expanduser(soundfilePath))

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
        sdifFile = pysdif.SdifFile(os.path.expanduser(sdifPath), 'r')
        
        #obtain the references to the streams in sdifFile
        streamRefs = sdifFile.get_stream_IDs()

        #the number of sound references  
        self.numRefs = len(streamRefs) 
        
        #assign a lookup for soundgrains
        self.soundgrains = {}
        for i, sg in enumerate(streamRefs):
            self.soundgrains[i+1] = OMPursuitSoundgrain(sg.source, downsampleFactor)
            
        #build the descriptor object (holds the raw data, might not need this as an attibute at a later stage...)
        #TODO: some of the frame types in the definition may not have data associated, so the table references should be built when reading the SDIF
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

        
        for g in self.soundgrains.values():
            g.globalValues = {}

        #read the descriptor values from the sdif and fill the lookup table
        for frame in sdifFile:
            for matrix in frame:
                if frame.signature == 'XGLB':
                    self.soundgrains[frame.id].globalValues[matrix.signature] = (matrix.get_data()[0])[0]
                else:  
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

        dtype = [(d, float) for d in self.descriptorLookup[1]['1WMN'].keys() if len(self.descriptorLookup[1]['1WMN'][d]['values']) > 0]
        dtype.append(('time', float))

        for i in range(1, self.numRefs+1):
            for q, ys in enumerate(maxy):
                if q == 0:
                    self.soundgrains[i].shortTimeDescriptors = np.zeros(ys, dtype=dtype)
                    dummyArray = self.soundgrains[i].shortTimeDescriptors
                    dummyString = '1DSC' 
                else:
                    self.soundgrains[i].averagedDescriptors = np.zeros(ys, dtype=dtype)
                    dummyArray = self.soundgrains[i].averagedDescriptors
                    dummyString = '1WMN'
 
                for j in range(ys):
                    checkTime = True
                    for key in [d[0] for d in dtype]:
                        if key != 'time':
                            if len(self.descriptorLookup[i][dummyString][key]['values']) > j:
                                dummyArray[j][key] =  self.descriptorLookup[i][dummyString][key]['values'][j]
                                if checkTime:   
                                    checkTime = False
                                    dummyArray[j]['time'] = self.descriptorLookup[i][dummyString][key]['time'][j]

        self.indices = set(self.soundgrains.keys()) #store the set of indices to apply the constriant operations later

    def len(self):
        return len(self.soundgrains.keys())

    def reinitWeights(self):
        for s in self.soundgrains.values():
            s.tempGain = 1.0    
