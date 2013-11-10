import numpy as np
import scipy.signal as sig
import scipy.linalg as linalg
import scikits.audiolab as audiolab
import pysdif
import os
import re
import operator
import time
from scipy.interpolate import interp1d

class OMPursuitAbstractConstraintContainer:
    '''Abstract base class for constraint containers'''
 
    def setConstraintValuesAndTimes(self): 

        constraintValues = {i:[] for i in range(1, self.numConstraints+1)}
        constraintTimes = {i:[] for i  in range(1, self.numConstraints+1)}

        #read the frames from the sdif file
        for frame in self.sdifFile:
            constraintTimes[frame.id].append(frame.time)
            for matrix in frame:  
                constraintValues[frame.id].append(matrix.get_data()[0][0])

        for key in constraintValues.keys():
            self.constraints[key].cValues = np.array(constraintValues[key])
            self.constraints[key].cTimes = np.array(constraintTimes[key])

class OMPursuitAbstractConstraint:
    '''Abstract base class for constraints'''

    def interpolatedValue(self, time, kind='linear'):
        try:
            f = interp1d(self.cTimes, self.cValues, kind=kind)
        except ValueError:
            return self.cValues[0]

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


class OMPAnalysisConstraintSet(OMPursuitAbstractConstraintContainer):

    def __init__(self, path):

        self.sdifFile = pysdif.SdifFile(os.path.expanduser(path), 'r')
        sc = self.sdifFile.get_stream_IDs()
        self.constraints = {}
        self.numConstraints = len(sc)

        #initialize the appropriate objects
        for i, constraint in enumerate(sc):
            C = OMPAnalysisConstraint(constraint.treeway)
            self.constraints[i+1] = C

        self.setConstraintValuesAndTimes()
 
             
class OMPAnalysisConstraint(OMPursuitAbstractConstraint):
    def __init__(self, constraintString):
        parsedConstraintString = re.split(' ', constraintString)
        self.cSignature = parsedConstraintString[1]   


class OMPursuitConstraint(OMPursuitAbstractConstraint):
    '''Represents a set or vector calculation on the collection of available soundgrains'''

    def __init__(self, constraintString):

        #Will use this later to build the expression from the fullconstraint
        self.cString = constraintString
        self.cString = '(' + self.cString + ')'

        operatorLookup = {'<' : operator.lt, '<=' : operator.le, '==' : operator.eq, '!=' : operator.ne, '>=' : operator.ge, '>' : operator.gt}

        parsedConstraintString = re.split(' ', constraintString)
        self.cSignature = parsedConstraintString[1]
        
        if parsedConstraintString[0] in operatorLookup.keys():
            self.cType = 'conditional'
            self.operator = operatorLookup[parsedConstraintString[0]]

        elif parsedConstraintString[0].lower() == 'w':
            self.cType = 'weight'

        elif parsedConstraintString[0].lower() == 'b':
            self.cType = 'bias'
                
        elif parsedConstraintString[0].isdigit():
            self.cType = 'k-nearest'
            self.kn = eval(parsedConstraintString[0])

        else:
            raise Exception        

        self.index = eval(parsedConstraintString[3])

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


class OMPursuitCompoundConstraint(OMPursuitAbstractConstraintContainer):
    '''Parse a constraint definition sdif and serve as a container for OMPusuitConstraint instances'''

    def __init__(self, constraintSdifPath):

        self.sdifFile = pysdif.SdifFile(os.path.expanduser(constraintSdifPath), 'r')
        sc = self.sdifFile.get_stream_IDs()
        self.numConstraints = len(sc)
        self.constraints = {}

        self.operatorLookup = {'and ' : 'self.andOMP', 'or ' : 'self.orOMP', 'xor ': 'self.xorOMP', 'nand ' : 'self.nandOMP', 'nor ' : 'self.norOMP', 'xnor ' : 'self.xnorOMP', 'b ' : 'self.biasOMP', 'w ' : 'self.weightOMP'}
        
        #obtain the full constraint expression
        for nvt in self.sdifFile.get_NVTs():
            if 'Fullconstraint' in nvt.keys():
                self.fullConstraintString = nvt['Fullconstraint']
                break

        print(self.fullConstraintString)
        
        #initialize the OMPursuiConstraintObjects
        for i, constraint in enumerate(sc):
            C = OMPursuitConstraint(constraint.treeway)
            self.constraints[i+1] = C

        self.setConstraintValuesAndTimes()

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


    #TODO: refactor, many of these are basically duplicates of functions from the operator module and are added for consistency 
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
        self.sdifFile = pysdif.SdifFile(os.path.expanduser(sdifPath), 'r')
        
        #obtain the references to the streams in sdifFile
        self.streamRefs = self.sdifFile.get_stream_IDs()

        #the number of sound references  
        self.numRefs = len(self.streamRefs) 
        self.sdifTypes = {}

        #assign a lookup for soundgrains
        self.soundgrains = {}
        for i, sg in enumerate(self.streamRefs):
            self.soundgrains[sg.numid] = OMPursuitSoundgrain(sg.source, downsampleFactor)
            self.soundgrains[sg.numid].corpusID = eval(sg.treeway)
 
        #build the descriptor object (holds the raw data, might not need this as an attibute at a later stage...)
        #TODO: some of the frame types in the definition may not have data associated, so the table references should be built when reading the SDIF
        self.descriptorLookup = {}
        for i in range(1, self.numRefs+1):
            self.descriptorLookup[i] = {}
            for ft in self.sdifFile.get_frame_types():
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
        for frame in self.sdifFile:
            for matrix in frame:
                if frame.signature == 'XGLB':
                    self.soundgrains[frame.id].globalValues[matrix.signature] = (matrix.get_data()[0])[0]
                else:  
                    self.descriptorLookup[frame.id][frame.signature][matrix.signature]['values'].append((matrix.get_data()[0]))
                    self.descriptorLookup[frame.id][frame.signature][matrix.signature]['time'].append(frame.time) 
                #if matrix.signature not in self.sdifTypes.keys():
                    #self.sdifTypes[matrix.signature] = matrix.desc

        #get the dimension in order to build the ndarrays
        maxy = [0, 0]
        
        for i, matsig in enumerate(['1DSC', '1WMN']):
            for k in range(1, self.numRefs+1):  
                for key in self.descriptorLookup[k][matsig].keys():
                    n = len(self.descriptorLookup[k][matsig][key]['time'])
                    if n > maxy[i]:
                        maxy[i] = n

        dtype = [(d, float) for d in self.descriptorLookup[1]['1WMN'].keys() if len(self.descriptorLookup[1]['1WMN'][d]['values']) > 0]
        dtype.append(('XCRP', int))
        dtype.append(('XDUR', float))
        dtype.append(('time', float))
        
        allGlbs = []
        for sgs in self.soundgrains.values():
            for key in sgs.globalValues.keys():
                if key not in allGlbs:
                    allGlbs.append(key)

        if 'XMDC' in allGlbs:
            dtype.append(('XMDC', int))

        if 'XVEL' in allGlbs:
            dtype.append(('XVEL', int))

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
                        if key not in ['time', 'XCRP', 'XDUR', 'XMDC', 'XVEL']:
                            if len(self.descriptorLookup[i][dummyString][key]['values']) > j:
                                dummyArray[j][key] =  self.descriptorLookup[i][dummyString][key]['values'][j]
                                if checkTime:   
                                    checkTime = False
                                    dummyArray[j]['time'] = self.descriptorLookup[i][dummyString][key]['time'][j]
                        elif key in ['XCRP', 'XDUR', 'XMDC', 'XVEL']: 
                            try: 
                                dummyArray[j][key] = self.soundgrains[i].globalValues[key]    
                            except KeyError:
                                dummyArray[j][key] = 0
  

        self.indices = set(self.soundgrains.keys()) #store the set of indices to apply the constriant operations later

    def len(self):
        return len(self.soundgrains.keys())

    def reinitWeights(self):
        for s in self.soundgrains.values():
            s.tempGain = 1.0    


class OMPursuitSegSignal:

    def signalSegment(self, timeinsec, length):
        return self.signal[np.floor(timeinsec * self.samplerate) : np.floor(timeinsec * self.samplerate) + length]

class OMPursuitTarget(OMPursuitSegSignal):
    '''Data representation for a single sound file target'''

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

        self.signal = x

    def signalSegment(self, timeinsec, length):
        return self.signal[np.floor(timeinsec * self.samplerate) : np.floor(timeinsec * self.samplerate) + length]

    def zeropadSignal(self, omdictionary, ommarkers):

        newsig = np.zeros(np.ceil(np.max(ommarkers.times) * self.samplerate) + max([len(s.signal) for s in omdictionary.soundgrains.values()]))

        if len(newsig) > len(self.signal): 
            newsig[0:len(self.signal)] = self.signal
            self.signal =  newsig

class OMPursuitMarkers:
    def __init__(self, sdifPath):

        sdifFile = pysdif.SdifFile(os.path.expanduser(sdifPath), 'r')
        self.times = []
        for frame in sdifFile:
            '''
            if frame.signature == 'XXXX':
                for matrix in frame:
                    self.times.append(matrix.get_data()[0])
            else:
                self.times.append(frame.time)'''
            self.times.append(frame.time)

        self.times = np.array(self.times)
        self.times = self.times[np.argwhere(self.times >= 0.0).flatten()]#sanity check

class OMPursuitModel(OMPursuitSegSignal):
    def __init__(self, datatype, length, vectorlength, samplerate):
        self.parameterArray = np.zeros(length, dtype=datatype)
        self.signal = np.zeros(vectorlength)
        self.samplerate = samplerate 

class OMPursuitAnalysis:

    def __init__(self, omdictionary, omcompoundconstraint, omtarget, ommarkers, ompanalysisconst, maxtotal):

        self.ompDictionary = omdictionary
        self.ompCompoundConstraint = omcompoundconstraint
        self.ompTarget = omtarget
        self.ompMarkers = ommarkers
        
        for value in ompanalysisconst.constraints.values():
            if value.cSignature == 'XMSA':
                self.maxSimultaneousAtoms = value
            elif value.cSignature == 'XMSC':
                self.maxSimultaneousCorpusAtoms = value
            elif value.cSignature == 'XMNT':
                self.minTimeDistance = value
            elif value.cSignature == 'XMXT':
                self.maxTimeDistance = value

        datatype = self.ompDictionary.soundgrains[1].averagedDescriptors.dtype.descr
        datatype.append(('mtime', '<f8'))
        datatype.append(('mcoef', '<f8'))
        datatype.append(('mindex', int))
        self.ompModel = OMPursuitModel(datatype, maxtotal, len(self.ompTarget.signal), self.ompTarget.samplerate)

        #Analysis constraints
        self.maxTotalSoundgrains = maxtotal
     
    def constrainedMP(self):
        '''Matching Pursuit with OMPursuit constraints'''

        totalCount = 0
        coefficients = np.zeros((len(self.ompMarkers.times), self.ompDictionary.len()))        
        cindices = np.zeros((len(self.ompMarkers.times), self.ompDictionary.len()), dtype=int)
        dummyTimes = self.ompMarkers.times.copy()
        subindices = np.arange(len(self.ompMarkers.times))
        
        while totalCount < self.maxTotalSoundgrains:
            print('The current iteration index is %d'%totalCount)

            #costly loop
            for n, time in enumerate(dummyTimes):
                for index in self.ompCompoundConstraint.cFullConstraintFunction(self.ompDictionary, time):
                    grain = self.ompDictionary.soundgrains[index].signal
                    coef = np.inner(grain, self.ompTarget.signalSegment(time, len(grain)))
                    if abs(coef) > coefficients[n, index-1]:
                        coefficients[n, index-1] = coef
                        cindices[n, index-1] = index

                self.ompDictionary.reinitWeights()

            #TODO:Here metaconstraints could be added, e.g. weight the coefficients based on polyphony (number of soundgrains at a location)

            validSoundgrain = False
            scoefinds = np.argsort(abs(coefficients.flatten()))[::-1]
            unravelShape = coefficients.shape

            for ii, i_ in enumerate(scoefinds):

                i = np.unravel_index(i_, unravelShape)#the 2d index that corresponds to the 1d sorted index

                if int(cindices[i]) == 0:
                    continue


                #avoid the same soundgrain at the same location
                indexwhere = set(np.argwhere(self.ompModel.parameterArray['mindex'][0:totalCount] == cindices[i]).flatten())
                timewhere = set(np.argwhere(abs(self.ompModel.parameterArray['mtime'][0:totalCount] - dummyTimes[i[0]]) < 0.0000000001).flatten())

                try:
                    indexwherecorpus = set(np.argwhere(self.ompModel.parameterArray['XCRP'][0:totalCount] == self.ompDictionary.soundgrains[cindices[i]].averagedDescriptors['XCRP'][0]).flatten())
                except ValueError:
                    indexwherecorpus = set([])

                #the max. simul. atoms constraint is satisfied if it exists
                try: 
                    a = (len(timewhere) <= self.maxSimultaneousAtoms.interpolatedValue(dummyTimes[i[0]]))
                except AttributeError:
                    a = True

                #the current index is already at the current time?
                b = (indexwhere & timewhere == set([]))

                #the max. simul. corpus atoms constraint is satisfied if it exists
                try:
                    c = (len(indexwherecorpus & timewhere) <= self.maxSimultaneousCorpusAtoms.interpolatedValues(dummyTimes[i[0]]))
                except AttributeError:
                    c = True

                #need to resolve the dynamic constraints here 
                if a and b and c:
                    validSoundgrain = True

                    #update the signal vectors
                    grain = self.ompDictionary.soundgrains[cindices[i]].signal
                    targseg = self.ompTarget.signalSegment(dummyTimes[i[0]], len(grain)) 
                    targseg -= grain * coefficients[i]

                    modseg = self.ompModel.signalSegment(dummyTimes[i[0]], len(grain))
                    modseg += grain * coefficients[i]
            
                    #store the parameters
                    for key in self.ompDictionary.soundgrains[cindices[i]].averagedDescriptors.dtype.names:
                        self.ompModel.parameterArray[totalCount][key] = self.ompDictionary.soundgrains[cindices[i]].averagedDescriptors[key][0]

                    self.ompModel.parameterArray[totalCount]['mtime'] = dummyTimes[i[0]]
                    self.ompModel.parameterArray[totalCount]['mcoef'] = coefficients[i]
                    self.ompModel.parameterArray[totalCount]['mindex'] = cindices[i]
 
                    #update the count
                    totalCount += 1
            
                    #remove the time points that violate the time constraints if they exist
                    try:
                        p = np.argwhere(abs(self.ompMarkers.times - dummyTimes[i[0]]) > self.minTimeDistance.interpolatedValue(dummyTimes[i[0]])).flatten()
                        p = np.intersect1d(subindices, p)
                        subindices = p
                        

                    except AttributeError:
                        p = None

                    try:
                        q = np.argwhere(abs(self.ompMarkers.times - dummyTimes[i[0]]) < self.maxTimeDistance.interpolatedValue(dummyTimes[i[0]])).flatten()
                        
                    except AttributeError:
                         q = None

                    if (p != None) and (q != None):
                        pq = np.union1d(p, q)
                    elif (p != None) and (q == None):
                        pq = p
                    elif (q != None) and (p == None):
                        pq = q
                    elif (q == None) and (p == None):
                        pq = np.arange(len(self.ompMarkers.times))


                    ntime = [self.ompMarkers.times[k] for k in pq]
                    ntime.append(dummyTimes[i[0]]) #don't exclude the current time point, i.e. simultaneous sgs 
                    dummyTimes = np.sort(np.array(ntime))
                    coefficients = np.zeros((len(dummyTimes), self.ompDictionary.len()))
                    cindices = np.zeros((len(dummyTimes), self.ompDictionary.len()), dtype=int)

                    break

                #need to check for valid time points to update (only those that intersect with where the previous sg was removed (optimization)
                
            if not validSoundgrain:
                print('No soundgrains satisfy the given constraints')
                break

        self.ompModel.parameterArray = self.ompModel.parameterArray[0:totalCount]    
       

    def writeModelSdif(self, path):

        iterationIndices = self.ompModel.parameterArray.argsort(order='mtime')
        self.ompModel.parameterArray.sort(order='mtime')

        f = pysdif.SdifFile('%s'%os.path.expanduser(path), 'w')
        f.add_NVT({'Date' : time.asctime(time.localtime()), 'Tablename' : 'FileInfo', 'Author': 'OM-Pursuit v0.1 - G. Boyes, M. Schumacher'})
        f.add_frame_type('XSGR', 'XSGR soundgrain-data')

        f.add_frame_type('XGLB', 'XDUR duration, XCRP Corpus-ID, XMDC Midi-cent, XVEL Midi-velocity, XNRM Signal-norm')

        f.add_matrix_type('XDUR', 'Duration')
        f.add_matrix_type('XCRP', 'Corpus-ID')
        f.add_matrix_type('XMDC', 'Midi-cent')
        f.add_matrix_type('XVEL', 'Midi-velocity')
        f.add_matrix_type('XNRM', 'Signal-norm')

        #the amplitude is 1./ norm * mp-coef 
        f.add_matrix_type('XSGR', 'Amplitude-coefficient, MP-coefficient, Iteration-index')

        
        #add the stream references from the dictionary
        for ref in self.ompDictionary.sdifFile.get_stream_IDs():
            f.add_streamID(ref.numid, ref.source, ref.treeway)
           
        #add the global data for the soundgrains in the model
        normLkp = {}
        for key in (set(self.ompDictionary.soundgrains.keys()) & set(np.unique(self.ompModel.parameterArray['mindex']))):
            sg = self.ompDictionary.soundgrains[key]
            for mkey in sg.globalValues:
                frame = f.new_frame('XGLB', 0.0, key)
                frame.add_matrix(mkey, np.array([[float(sg.globalValues[mkey])]]))
                frame.write()
            frame = f.new_frame('XGLB', 0.0, key)
            frame.add_matrix('XNRM', np.array([[sg.norm]]))
            normLkp[key] = sg.norm
            frame.write()

        #add the time-varying analysis data
        for i, row in enumerate(self.ompModel.parameterArray):
             frame = f.new_frame('XSGR', row['mtime'], row['mindex'])
             frame.add_matrix('XSGR', np.array([[1.0 / normLkp[row['mindex']] * row['mcoef']], [row['mcoef']], [iterationIndices[i]]]))
             frame.write()

        f.close() 
        

