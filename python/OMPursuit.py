import numpy as np
import scipy.signal as sig
import scipy.linalg as linalg
import scikits.audiolab as audiolab
import pysdif
import os



'''
class OMPursuitConstraint:
    def __init__(self):

class OMPursuitCompoundConstraint:
    def __init__(self):
'''        
class OMPursuitSoundgrain:
    def __init__(self, soundfilePath, downsampleFactor):

        sformats = {'.aif' : audiolab.aiffread, '.aiff' : audiolab.aiffread, '.wav' : audiolab.wavread}
        readFunc = sformats[(os.path.splitext(soundfilePath)[1]).lower()]
        x, fs, p = readFunc(soundfilePath)

        if len(np.shape(x)) > 1:
            x = x[:, 0]

        self.norm = linalg.norm(x)#the original norm
        self.samplerate = fs            

        if downsampleFactor > 1:
            x = sig.resample(x, len(x)/downsampleFactor)
            self.samplerate = fs / downsampleFactor
        elif downsampleFactor < 1:
            raise Exception("Upsampling not permitted")

        self.signal = x / linalg.norm(x) #the normalization step occurs once'''

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

    def len(self):
        return len(self.soundgrains.keys())
