#cython: embedsignature=True

from libc.stdio cimport *
from libc.stdlib cimport *

cdef extern from "string.h":
    ctypedef void* const_void_ptr "const void *"
    void *memcpy(void *s1, const_void_ptr s2, size_t n)
    void import_array()

import os.path
import numpy
cimport numpy as c_numpy
from numpy cimport ndarray, npy_intp
from cpython cimport PyString_FromStringAndSize

cdef extern from "numpy/arrayobject.h":
    PyArray_SimpleNew(int nd, npy_intp *dims, int type_num)
    PyArray_SimpleNewFromData(int nd, npy_intp *dims, int type_num, void *data)

from pysdif cimport *

DEF _SdifNVTStreamID = 0xfffffffd

# ----------------------------------------------------------------------------------

cdef int SDIF_CLOSED = 2
cdef int _g_sdif_initiated = 0

cdef dict _SDIF_FILEMODE = {
    'r' : eReadFile,
    'w' : eWriteFile,
    'rw': eReadWriteFile,
    'wr': eReadWriteFile
}

cdef dict _SDIF_DATATYPES = {
    0x0301:"char",
    0x0004:"float32",
    0x0008:"float64",
    0x0101:"int8",
    0x0102:"int16",
    0x0104:"int32",
    0x0108:"int64",
    0x0201:"uint8",
    0x0202:"uint16",
    0x0204:"uint32",
    0x0208:"uint64",
    }

cdef dict _SDIF_TO_NUMPY_TYPENUM = {
    0x0301:c_numpy.NPY_BYTE,
    0x0004:c_numpy.NPY_FLOAT,
    0x0008:c_numpy.NPY_DOUBLE,
    0x0101:c_numpy.NPY_BYTE,
    0x0102:c_numpy.NPY_SHORT,
    0x0104:c_numpy.NPY_INT,
    0x0108:c_numpy.NPY_LONG,
    0x0201:c_numpy.NPY_UBYTE,
    0x0202:c_numpy.NPY_USHORT,
    0x0204:c_numpy.NPY_UINT,
    0x0208:c_numpy.NPY_ULONG
    }
    
cdef inline int dtype_sdif2numpy(int typenum):
    if   typenum == 0x0004: typenum = c_numpy.NPY_FLOAT
    elif typenum == 0x0301: typenum = c_numpy.NPY_BYTE
    elif typenum == 0x0008: typenum = c_numpy.NPY_DOUBLE
    elif typenum == 0x0104: typenum = c_numpy.NPY_INT
    elif typenum == 0x0101: typenum = c_numpy.NPY_BYTE
    elif typenum == 0x0102: typenum = c_numpy.NPY_SHORT
    elif typenum == 0x0108: typenum = c_numpy.NPY_LONG
    elif typenum == 0x0201: typenum = c_numpy.NPY_UBYTE
    elif typenum == 0x0202: typenum = c_numpy.NPY_USHORT
    elif typenum == 0x0204: typenum = c_numpy.NPY_UINT
    elif typenum == 0x0208: typenum = c_numpy.NPY_ULONG
    return typenum
    
cdef inline int dtype_numpy2sdif(int dtype):
    if   dtype == c_numpy.NPY_FLOAT : dtype = 0x0004
    elif dtype == c_numpy.NPY_BYTE  : dtype = 0x0301
    elif dtype == c_numpy.NPY_DOUBLE: dtype = 0x0008
    elif dtype == c_numpy.NPY_INT   : dtype = 0x0104  
    elif dtype == c_numpy.NPY_BYTE  : dtype = 0x0101  
    elif dtype == c_numpy.NPY_SHORT : dtype = 0x0102  
    elif dtype == c_numpy.NPY_LONG  : dtype = 0x0108  
    elif dtype == c_numpy.NPY_UBYTE : dtype = 0x0201  
    elif dtype == c_numpy.NPY_USHORT: dtype = 0x0202      
    elif dtype == c_numpy.NPY_UINT  : dtype = 0x0204  
    elif dtype == c_numpy.NPY_ULONg : dtype = 0x0208
    return dtype
    
cdef inline unsigned int _str2sig (char *sig):
   return ((((<unsigned int>(sig[0])) & 0xff) << 24) | 
       (((<unsigned int>(sig[1])) & 0xff) << 16) | 
       (((<unsigned int>(sig[2])) & 0xff) << 8) | 
       ((<unsigned int>(sig[3])) & 0xff))
       
def signature2str(SdifSignature signature):
    return PyString_FromStringAndSize(SdifSignatureToString(signature), 4)

cdef inline sig2str(SdifSignature signature):
    return PyString_FromStringAndSize(SdifSignatureToString(signature), 4)
    
def str2signature(s):
    return _str2sig(s)

cdef inline ndarray _array_from_matrix_data_no_copy(SdifMatrixDataT *matrix): # , int copy=0):
    cdef SdifMatrixHeaderT *header = matrix.Header
    cdef npy_intp dims[2]
    dims[0] = <npy_intp>header.NbRow
    dims[1] = <npy_intp>header.NbCol
    return PyArray_SimpleNewFromData(2, dims, dtype_sdif2numpy(header.DataType), matrix.Data.Void)


cdef inline ndarray _array_from_matrix_data_copy(SdifMatrixDataT *matrix): 
    cdef ndarray out
    cdef SdifMatrixHeaderT *header = matrix.Header
    cdef npy_intp dims[2]
    dims[0] = <npy_intp>header.NbRow
    dims[1] = <npy_intp>header.NbCol
    out = PyArray_SimpleNew(2, dims, dtype_sdif2numpy(header.DataType)) 
    memcpy(<void *>out.data, matrix.Data.Void, c_numpy.PyArray_ITEMSIZE(out) * dims[0] * dims[1])
    return out

cdef inline SdifSignature as_signature(sig):
    if isinstance(sig, str):
        return _str2sig(sig)
    else:
        return sig
        
cdef inline PyString_from_SdifString(SdifStringT *s):
    """ convert a SdifStringT to a python string """
    return PyString_FromStringAndSize(s.str, s.SizeW)
    
cdef inline SdifStringT *SdifString_from_PyString(bytes s):
    cdef char *bytes = s
    cdef SdifStringT *sdifstr = SdifStringNew()
    SdifStringAppend(sdifstr, bytes)
    return sdifstr
    
cdef inline nvt_to_dict( SdifNameValueTableT *nvt ):
    cdef int i
    cdef SdifUInt4      iNV
    cdef SdifHashNT     *pNV
    cdef SdifHashTableT *HTable
    cdef SdifNameValueT *namevalue
    
    HTable = nvt.NVHT
    table = {}
    for i in range(HTable.HashSize):
        pNV = HTable.Table[i]
        while pNV:
            namevalue = <SdifNameValueT *>(pNV.Data)
            name = namevalue.Name
            value = namevalue.Value
            table[name] = value
            pNV = pNV.Next
    return table
    
cdef valuetables_to_dicts (SdifNameValuesLT *namevalues):
    """ create a list of dicts where each dict represents a value table """
    SdifListInitLoop(namevalues.NVTList)
    tables = []
    while SdifListIsNext(namevalues.NVTList):
        namevalues.CurrNVT = <SdifNameValueTableT *>(SdifListGetNext(namevalues.NVTList))
        tables.append(nvt_to_dict(namevalues.CurrNVT))
    return tables
    
cdef streamidtable_to_list(SdifStreamIDTableT *table):
    cdef unsigned int i
    cdef SdifHashNT *pID
    cdef SdifStreamIDT *streamid
    streams = []
    for i in range(table.SIDHT.HashSize):
        pID = table.SIDHT.Table[i]
        while pID:
            streamid = <SdifStreamIDT *>(pID.Data)
            streamid_w = StreamID_fromSdifStreadIDT(streamid)
            streams.append(streamid_w)
            pID = pID.Next
    return streams
            
cdef class StreamID:
    cdef SdifStreamIDT *this
    cdef int own_this
    property numid:
        def __get__(self): return self.this.NumID
    property source:
        def __get__(self): return self.this.Source
    property treeway:
        def __get__(self): return self.this.TreeWay
    def __cinit__(self, int numid, source, treeway):
        if numid == -1:
            # we are a wrapper of an existing SdifStreamID
            self.own_this = 0
        else:
            self.this = SdifCreateStreamID(<SdifUInt4>numid, source, treeway)
            self.own_this = 1
    def __dealloc__(self):
        if self.own_this:
            SdifKillStreamID(self.this)
    def __repr__(self):
        return "StreamID(numid=%d, source='%s', treeway='%s')" % (
            self.numid,
            self.source,
            self.treeway)            
        
cdef StreamID_fromSdifStreadIDT(SdifStreamIDT *this):
    cdef StreamID out = StreamID(-1, None, None)  # create a wrapper
    out.this = this
    out.own_this = 0
    return out
    
cdef SdifMatrixTypeT *MatrixType_create(signature, column_names):
    cdef SdifSignature sig = _str2sig(signature)
    cdef SdifMatrixTypeT *mt = SdifCreateMatrixType(sig, NULL)
    for column_name in column_names:
        SdifMatrixTypeInsertTailColumnDef(mt, column_name)
    return mt
    

cdef SdifFrameTypeT *FrameType_create(signature, components):
    cdef SdifSignature sig = str2signature(signature)
    cdef SdifFrameTypeT *ft = SdifCreateFrameType(sig, NULL)
    for component in components:
        if isinstance(component, Component):
            SdifFrameTypePutComponent(ft, str2signature(component.signature), component.name)
        else:
            SdifFrameTypePutComponent(ft, str2signature(component[0]), component[1])
    return ft
    
cdef class MatrixTypeDefinition:
    cdef public object signature
    cdef public list column_names
    def __init__(self, signature, column_names):
        self.signature = signature
        self.column_names = column_names
    def __repr__(self):
        return "1MTD(signature=%s, column_names=%s)" % (self.signature, ', '.join(self.column_names))
    def __iter__(self):
        return iter(self.column_names)
    def __len__(self):
        return len(self.column_names)
    cdef SdifMatrixTypeT *toSdifMatrixType(self):
        return MatrixType_create(self.signature, self.column_names)
        
cdef MatrixTypesTable_to_list(SdifHashTableT *t):
    cdef unsigned int i
    cdef SdifHashNT *pName
    cdef SdifMatrixTypeT *matrix
    cdef SdifColumnDefT *column_def
    cdef list out = []
    cdef list column_names
    for i in range(t.HashSize):
        pName = t.Table[i]
        while pName:
            matrix = <SdifMatrixTypeT *>(pName.Data)
            if not SdifListIsEmpty(matrix.ColumnUserList):
                signature = sig2str(matrix.Signature)
                column_def = <SdifColumnDefT *>SdifListGetHead(matrix.ColumnUserList)
                column_names = []
                column_names.append(column_def.Name)
                while SdifListIsNext(matrix.ColumnUserList):
                    column_def = <SdifColumnDefT *>(SdifListGetNext(matrix.ColumnUserList))
                    column_names.append(column_def.Name)
                row = MatrixTypeDefinition(signature=signature, column_names=column_names)
                out.append(row)
            pName = pName.Next
    return out
        
cdef class Component:
    cdef readonly str signature
    cdef readonly str name
    cdef readonly unsigned int num
    def __init__(self, str signature, str name):
        self.signature = signature
        self.name = name
        self.num = 0
    def __repr__(self):
        return "Component(sig=%s, name=%s)" % (self.signature, self.name)
    
cdef Component Component_from_SdifComponent(SdifComponentT *c):
    cdef Component out = Component(sig2str(c.MtrxS), c.Name)
    out.num = c.Num
    return out
    
cdef FrameTypesTable_to_list(SdifHashTableT *t):
    cdef unsigned int i
    cdef SdifHashNT *pName
    cdef SdifFrameTypeT *frame
    cdef list out = []

    for i in range(t.HashSize):
        pName = t.Table[i]
        while pName:
            frame = <SdifFrameTypeT *>(pName.Data)
            if frame.NbComponentUse > 0:
                components = []
                signature = sig2str(frame.Signature)
                for j in range(frame.NbComponent - frame.NbComponentUse + 1, frame.NbComponent + 1):
                    component = Component_from_SdifComponent(SdifFrameTypeGetNthComponent(frame, j))
                    components.append(component)
                out.append(FrameTypeDefinition(signature, components))
            pName = pName.Next
    return out
    
cdef class FrameTypeDefinition:
    cdef public str signature
    cdef public list components
    def __init__(self, signature, components):
        self.signature = signature
        self.components = components
    def __repr__(self):
        s = [str(component) for component in self.components]
        return "1FTD (signature='%s', components=[%s])" % (self.signature, ', '.join(s))
    def __iter__(self):
        return iter(self.components)
    def __len__(self):
        return len(self.components)
    
# ----------------------------------------------------------------------

SDIF_NEWTYPES = {
    'RBEP': {
        'frame_types':[('RBEP', 'RBEP ReassignedBandEnhancedPartials')],
        'matrix_types':[('RBEP', 'Index, Frequency, Amplitude, Phase, Bandwidth, Offset')]
        }
}

def get_predefined_frames_and_matrices(sdiftype):
    t = SDIF_NEWTYPES.get(sdiftype)
    if t:
        return t['frame_types'], t['matrix_types']
    else:
        return None
    
# ----------------------------------------------------------------------

def _init():
    """initialize the sdif library (SdifGenInit) and initialize the numpy API"""
    global _g_sdif_initiated
    import_array()
    if _g_sdif_initiated == 0:
        _g_sdif_initiated = 1
        type_definitions_file = os.path.split(__file__)[0] + os.path.sep + "SdifTypes.STYP"
        if os.path.exists(type_definitions_file):
            SdifGenInit(type_definitions_file)
        else:
            SdifGenInit("")
        print "Sdif initialized"
    return _g_sdif_initiated 
        
def _cleanup():
    SdifGenKill()
    
# Forward declarations
cdef class FrameR
cdef class SdifFile

# enums to keep state of reading
cdef enum MatrixStatusE:
    eMatrixInvalid,
    eMatrixNothingRead,
    eMatrixHeaderRead,
    eMatrixDataRead,
    eMatrixDataSkipped,
    eMatrixOffline

cdef enum FrameStatusE:
    eFrameInvalid,
    eFrameNothingRead,
    eFrameHeaderRead,
    eFrameSomeDataRead,
    eFrameAllDataRead,
    eFrameSignatureRead

cdef enum SdifStatusE:
    eSdifNothing,
    eSdifGeneralHeader,
    eSdifAllASCIIChunks,
    eSdifSignature,
    eSdifFrameHeader,
    eSdifMatrixHeader,
    eSdifMatrixData

# -----------------------------------------------------------------------------------------------

cdef class Matrix:
    """
    Matrix is only a placeholder class to
    iterate through data while reading a SdifFile
    
    in particular the default behaviour is that
    when you are given a Matrix, this is only valid
    until a new one is read.
    
    See the methods 'get_data' and 'copy' for a better
    explanation of how to make the data in the Matrix 
    persistent
    """
    #cdef SdifMatrixHeaderT *header
    cdef SdifFileT *source_this
    cdef SdifSignature _signature
    cdef SdifFile source
    cdef MatrixStatusE status
    cdef ndarray data

    def __cinit__(self, SdifFile source):
        if source is not None:
            self.source = source
            #self.header = source.this.CurrMtrxH
            self.source_this = source.this
            self.data = None
        else:
            #self.header = NULL
            self.source_this = NULL
            self.status = eMatrixOffline
            
    property rows:
        def __get__(self): 
            #if self.header:
            if self.source_this:
                #return self.header.NbRow
                return self.source_this.CurrMtrxH.NbRow
            else:
                assert self.status == eMatrixOffline
                return len(self.data)
    property cols:
        def __get__(self): 
            #if self.header:
            if self.source_this:
                #return self.header.NbCol
                return self.source_this.CurrMtrxH.NbCol
            else:
                assert self.status == eMatrixOffline
                return len(self.data[0])
    property dtype:
        def __get__(self): 
            #if self.header:
            if self.source_this:
                return _SDIF_DATATYPES[self.source_this.CurrMtrxH.DataType]
            else:
                return self.data.dtype
    property signature:
        def __get__(self): 
            #if self.header:
            if self.source_this:
                return sig2str(self.source_this.CurrMtrxH.Signature)
            else:
                return sig2str(self._signature)
    property numerical_signature:
        def __get__(self): 
            #if self.header:
            if self.source_this:
                return self.source_this.CurrMtrxH.Signature
            else:
                return self._signature
                
    def get_data(self, copy=True):
        """
        read the data from the matrix as a numpy array
        
        NB: if copy is False, the data is not copied to the array, 
        the array is only a 'view' of this data and does not own it,
        so it is only valid until you read a new matrix. 
        If you want to keep the data, do 
        data = matrix.get_data().copy()
        """
        if self.source.matrix_status == eMatrixHeaderRead:
            self.source.read_matrix_data()
        if copy:
            return _array_from_matrix_data_copy(self.source_this.CurrMtrxData)
        return _array_from_matrix_data_no_copy(self.source_this.CurrMtrxData)
    def copy(self):
        """
        create a copy of the current matrix with the data
        already read and owned (placed in new_matrix.data). 
        This copied matrix does not get invalidated when a 
        new matrix is read
        """
        cdef Matrix new = Matrix(None)
        new.data = self.get_data().copy()
        new._signature = self.source_this.CurrMtrxH.Signature
        return new
    def skip(self):
        """
        skip the matrix.
        if only header was read, tell the Frame to skip the data
        """
        if self.status == eMatrixHeaderRead:
            # data has not been read yet
            self.source.skip_matrix_data()
    def __repr__(self):
        return "Matrix(sig=%s, rows=%d, cols=%d, dtype=%s)" % (self.signature, self.rows, self.cols, str(self.dtype))

            
# -----------------------------------------------------------------------------------------------

cdef class FrameR:
    """
    FrameR is really an iterator class,
    to be able to iterate through matrices. FrameR
    keeps a connection to its source SdifFile but all
    managing and indexing is done by the SdifFile, to
    keep things central. In particular, the matrices 
    resulting from calling its 'next()' method (or iterating
    with 'for') have no connection to the FrameR and talk 
    directly to the SdifFile, which, eventually, updates
    the FrameR if needed.
    It is only used for reading and, in particular, one
    would never create a FrameR but receive it as result of
    iterating through a SdifFile
    """
    cdef SdifFrameHeaderT *header
    cdef SdifFile source
    cdef SdifFileT *source_this
    
    def __cinit__(self, SdifFile source):
        self.source = source
        self.source_this = source.this
        self.header = source.this.CurrFramH
    def __dealloc__(self):
        self.source = None
        
    property signature:
        def __get__(self): #return sig2str(self.header.Signature)
            return sig2str(self.source_this.CurrFramH.Signature)
    property numerical_signature:
        def __get__(self): return self.source_this.CurrFramH.Signature
    property size:
        def __get__(self): return self.source_this.CurrFramH.Size
    property num_matrices:
        def __get__(self): return self.source_this.CurrFramH.NbMatrix
    def __len__(self):
        return self.header.NbMatrix
    property id:
        def __get__(self): return self.source_this.CurrFramH.NumID
    property time:
        def __get__(self): return self.source_this.CurrFramH.Time
    property matrix_idx:
        def __get__(self): return self.source.matrix_idx
    def matrix_exists(self, signature):
        cdef SdifSignature sig = as_signature(signature)
        # TODO 
    def __iter__(self):
        self.source.frame_status = eFrameSomeDataRead
        return self
    def __next__(self):
        """
        read next matrix and update the index
        NB: only the header of the matrix will be read, to get the data
        you must call the method matrix.get_data() on the resulting Matrix
        """
        # cdef SdifFile source = self.source
        if self.source.matrix_idx == self.source_this.CurrFramH.NbMatrix:
            raise StopIteration
        if self.source.matrix_status == eMatrixHeaderRead:
            SdifFSkipMatrixData(self.source_this)
        SdifFReadMatrixHeader(self.source_this)
        self.source.matrix_idx += 1
        self.source.matrix_status = eMatrixHeaderRead
        return self.source.matrix
    def get_matrix_data(self, copy=True):
        """
        optimized method for frames which only have one matrix.
        
        example:
        
        for frame in sdiffile:
            if frame.signature == "1TRC":
                data = frame.get_matrix_data()  # no need to iterate through matrices in 1TRC files
        """
        SdifFReadMatrixHeader(self.source_this)
        SdifFReadMatrixData(self.source_this)
        self.source.matrix_idx = 1
        self.source.matrix_status = eMatrixDataRead
        self.source.frame_status = eFrameAllDataRead
        if copy:
            return _array_from_matrix_data_copy(self.source_this.CurrMtrxData)
        return _array_from_matrix_data_no_copy(self.source_this.CurrMtrxData) 
        
cdef class FrameW:
    """
    FrameW : class to write frames to a SdifFile
    
    one does not create instances of this class directly but
    you get an instance by calling
    frame_to_write = sdiffile.new_frame(signature, time, streamID)
    
    then:
    frame_to_write.add_matrix(signature, numpy_array)
    
    and last:
    frame_to_write.write()   # this writes it to the sdiffile
    """
    cdef SdifFile sdiffile
    cdef SdifSignature signature
    cdef SdifFloat8 time
    cdef SdifUInt4 streamID
    cdef list matrices
    cdef list signatures
    cdef SdifUInt4 frame_size
    cdef SdifUInt4 num_matrices
    def __repr__(self):
        return "FrameW(signature=%s, time=%f, streamID=%d)" % (
            sig2str(self.signature),
            self.time,
            self.streamID)
    def add_matrix(self, signature, c_numpy.ndarray data_array):
        if data_array.ndim == 1:
            data_array.resize((data_array.shape[0], 1))
        self.signatures.append(signature)
        self.matrices.append(data_array)
        self.num_matrices += 1
        self.frame_size += SdifSizeOfMatrix(
            #<SdifDataTypeET>(dtype_numpy2sdif(data_array.dtype.type_num)),
            #<SdifDataTypeET>(dtype_numpy2sdif(data_array.dtype.num)),
            <SdifDataTypeET>(dtype_numpy2sdif(data_array.descr.type_num)),
            data_array.shape[0], data_array.shape[1]) # rows, cols
    def write(self):
        """
        write the current frame to disk. This function is called
        after add_matrix has been called (if there are any matrices in
        the current frame). The frame is written all at once. 
        """
        cdef SdifUInt4 fsz
        cdef SdifSignature matrix_sig
        cdef int dtype
        cdef str signature
        cdef c_numpy.ndarray matrix
        cdef int i
        SdifFSetCurrFrameHeader(self.sdiffile.this, self.signature, self.frame_size, 
            self.num_matrices, self.streamID, self.time)
        fsz = SdifFWriteFrameHeader(self.sdiffile.this)
        for i in range(self.num_matrices):
            matrix_sig = _str2sig(self.signatures[i])
            matrix = self.matrices[i]
            #dtype = dtype_numpy2sdif(matrix.dtype.num)
            dtype = dtype_numpy2sdif(matrix.descr.type_num)
            fsz += SdifFWriteMatrix(self.sdiffile.this, matrix_sig, 
                <SdifDataTypeET>dtype, 
                matrix.shape[0], matrix.shape[1],   # rows, cols
                matrix.data)
        self.sdiffile.write_status = eSdifMatrixData

cdef FrameW FrameW_new(SdifFile sdiffile, SdifSignature sig, SdifFloat8 time, SdifUInt4 streamID=0):
    cdef FrameW f = FrameW()
    f.sdiffile = sdiffile
    f.signature = sig
    f.time = time
    f.streamID = streamID
    f.matrices = []
    f.signatures = []
    f.frame_size = SdifSizeOfFrameHeader()
    f.num_matrices = 0
    return f

            
# -----------------------------------------------------------------------------------------------
    
cdef class SdifFile:
    """
    SdifFile(filename, mode="r")
    
    mode is "r":read | "w": write | "wr" or "rw":read-write
    
    this is the low level interface for reading and writing sdif files
    it mirrors the sdif library quite transparently so that the example files
    and utilities using it can be directly translated with it. in particular
    it does not create any intermediate objects, even the data of the matrices
    is a numpy array mapped to the c array read from disk, so no allocation takes
    place. 
    
    to read for ex. 1TRC format:
    
    sdif_file = SdifFile('filename.sdif')
    sig1TRC = str2signature("1TRC")
    while not sdif_file.eof:
        sdif_file.read_frame_header()
        if sdif_file.frame_numerical_signature) == sig1TRC:
            print sdif_file.time
            for n in range(sdif_file.matrices_in_frame):
                sdif_file.read_matrix_header()
                if sdif_file.matrix_numerical_signature == sig1TRC:
                    data = sdif_file.get_matrix_data() 
                    # data is now a numpy array but you must copy the data if 
                    # you intend to keep it after you have read the matrix.
                    # One you read a new matrix, this data will be no longer valid
                    print data
        
    a more natural way:
    
    sdif_file = SdifFile('filename.sdif')
    for frame in sdif_file:
        if frame.signature == "1TRC":
            print frame.time
            for matrix in frame:
                if matrix.signature == "1TRC":
                    print matrix.get_data()
    
    the frames and the matrices resulting from the iteration
    are only guaranteed to be valid as long as no new frames and matrices are read
    
    to write a SdifFile:
    
    f = SdifFile('new_sdif.sdif', 'w')
    # these are optional
    f.add_NVT({
        'name' : 'my name',
        'date' : time.asctime(time.localtime())
    })
    f.add_frame_type('1NEW', '1ABC NewMatrix, 1FQ0 New1FQ0')
    f.add_matrix_type('1ABC', 'Column1, Column2')
    # now you can begin adding frames
    frame = f.new_frame('1NEW', time_now)
    frame.add_matrix('1ABC', array([
        [0,     1.2],
        [3.5,   8.13],
        ...
        ]))
    frame.write()
    
    # say we just want to take the data from an existing
    # sdiffile, modify it and write it back
    in_sdif = SdifFile("existing-file.sdif")
    out_sdif = SdifFile("outfile.sdif", "w")
    out_sdif.clone_definitions(in_sdif)
    for in_frame in in_sdif:
        if in_frame.signature == "1NEW":
            new_frame = out_sdif.new_frame("1NEW", in_frame.time)
            in_data = in_frame.get_matrix_data() # we know there is only one matrix
            # multiply the second column by 0.5
            in_data[:,1] *= 0.5
            new_frame.add_matrix('1ABC', in_data)
            new_frame.write()
    """
    
    cdef SdifFileT *this
    cdef readonly int eof
    cdef FrameR frame
    cdef Matrix matrix
    cdef FrameStatusE frame_status
    cdef size_t matrix_idx
    cdef MatrixStatusE matrix_status
    cdef SdifStatusE write_status

    def __cinit__(self, filename, mode="r", predefined_type=None):
        #assert mode in _SDIF_FILEMODE
        if mode == "r":
            if not os.path.exists(filename):
                raise RuntimeError("path not found, cannot open sdif file for reading")
        self.this = SdifFOpen(filename, _SDIF_FILEMODE[mode])
        
    def __init__(self, filename, mode="r", predefined_type=None):
        if self.mode == eReadFile:
            self.init_read()
            if not self.get_frame_types() and self.signature in SDIF_NEWTYPES:
                self.add_predefined_type(self.signature)
        elif self.mode == eWriteFile:
            self.init_write()
            if predefined_type is not None and predefined_type in SDIF_NEWTYPES:
                self.add_predefined_type(predefined_type)

    cdef void init_read(self):
        SdifFReadGeneralHeader (self.this)  
        SdifFReadAllASCIIChunks(self.this)  
        self.eof = (self.this.CurrSignature == eEof)
        self.this.TextStream = stdout  # this is needed by the print functions
        self.init_containers()
        self.frame_status = eFrameSignatureRead
        self.matrix_idx = 0
        self.matrix_status = eMatrixNothingRead
        
    cdef void init_containers(self):
        self.frame = FrameR(self)
        self.matrix = Matrix(self)
        
    cdef void init_write(self):
        SdifFWriteGeneralHeader(self.this)
        self.write_status = eSdifGeneralHeader
        
    def __dealloc__(self):
        self.frame = None
        self.close()  # fails silently if already closed
    
    def close(self):
        if self.eof == SDIF_CLOSED:
            return
        # make sure we write the global header even if no
        # new frames are written (the global chunks are automatically
        # written when a new frame is added to the sdiffile)
        if self.mode == eWriteFile:
            if self.write_status == eSdifGeneralHeader:
                self.write_all_ascii_chunks()
        SdifFClose(self.this)
        self.eof = SDIF_CLOSED

    property name:
        def __get__(self): return self.this.Name
    property mode:
        def __get__(self): return self.this.Mode
    property is_seekable:
        def __get__(self): return self.this.isSeekable
    property numerical_signature:
        def __get__(self): return self.this.CurrSignature
    property signature:
        def __get__(self): return sig2str(self.this.CurrSignature)
    property prev_time:
        def __get__(self): return self.this.PrevTime
    property frame_pos:
        def __get__(self): return self.this.CurrFramPos
    property matrix_cols:
        def __get__(self): return self.this.CurrMtrxH.NbCol
    property matrix_rows:
        def __get__(self): return self.this.CurrMtrxH.NbRow
    property matrix_data_type:
        def __get__(self): return self.this.CurrMtrxH.DataType
    property matrices_in_frame:
        def __get__(self): return self.this.CurrFramH.NbMatrix
    property frame_id:
        def __get__(self): return self.this.CurrFramH.NumID
    property pos:
        def __get__(self): 
            cdef SdiffPosT _pos
            SdifFGetPos(self.this, &_pos)
            return _pos
        def __set__(self, long pos): SdifFSetPos(self.this, &pos)
    property frame_numerical_signature:
        def __get__(self): return self.this.CurrFramH.Signature
    property frame_signature:
        def __get__(self): return sig2str(self.this.CurrFramH.Signature)
    property matrix_numerical_signature:
        def __get__(self): return self.this.CurrMtrxH.Signature
    property matrix_signature:
        def __get__(self): return sig2str(self.this.CurrMtrxH.Signature)
    property time:
        def __get__(self): return self.this.CurrFramH.Time
    property last_error:
        def __get__(self):
            cdef SdifErrorT* error = SdifFLastError(self.this)
            if error == NULL:
                return None
            return (error.Tag, error.Level)
    property frame_is_selected:
        def __get__(self): return SdifFCurrFrameIsSelected (self.this) 
    property matrix_is_selected:
        def __get__(self): return SdifFCurrMatrixIsSelected (self.this)
    property number_of_NVTs:
        def __get__(self):
            if SdifNameValuesLIsNotEmpty(self.this.NameValues):
                return SdifFNameValueNum(self.this)
            else:
                return 0
    def read_frame_header(self):
        """
        low level interface.
        read the frame header
        """
        self.finalize_frame()
        if self.eof == 1:
            return 0
        self.frame_status = eFrameHeaderRead
        return SdifFReadFrameHeader(self.this)
    def skip_frame_data(self):
        """
        low level interface.
        skip the frame with all the matrices it may contain.
        """
        assert self.frame_status == eFrameHeaderRead
        self.frame_status = eFrameAllDataRead
        cdef size_t bytes_read = SdifFSkipFrameData (self.this)
        self.finalize_frame()
        return bytes_read
    
    cdef inline void finalize_frame(self):
        # here we must make sure that we have read
        # the whole data before we try to 
        # read the signature (or eof) of the next frame
        cdef int i
        cdef size_t bytes_read
        cdef int status = self.frame_status
        if status == eFrameSignatureRead:
            return
        elif status == eFrameHeaderRead:
            SdifFSkipFrameData(self.this)
        elif status == eFrameSomeDataRead:
            if self.matrix_status < eMatrixDataRead:
                SdifFSkipMatrixData(self.this)
            for i in range(self.matrix_idx, self.this.CurrFramH.NbMatrix):
                SdifFSkipMatrix(self.this)
        self.matrix_idx = 0
        self.matrix_status = eMatrixNothingRead
        self.frame_status = eFrameNothingRead
        self.eof = SdifFGetSignature(self.this, &bytes_read) == eEof
    
    cdef inline void read_signature(self):
        cdef size_t NbCharRead
        self.eof = SdifFGetSignature(self.this, &NbCharRead) == eEof
        self.frame_status = eFrameSignatureRead
    
    def read_matrix_header(self):
        """
        low level interface.
        read the matrix header (signature, number of rows and columns, etc.)
        return the number of bytes read or 0 if no more matrices
        """
        if self.matrix_idx == self.this.CurrFramH.NbMatrix:
            return 0
        self.frame_status = eFrameSomeDataRead
        self.matrix_status = eMatrixHeaderRead
        self.matrix_idx += 1
        return SdifFReadMatrixHeader(self.this)
    
    cdef inline void read_matrix_data(self):
        self.matrix_status = eMatrixDataRead
        SdifFReadMatrixData(self.this)
        if self.matrix_idx == self.this.CurrFramH.NbMatrix:
            self.frame_status = eFrameAllDataRead
    
    def skip_matrix_data(self):
        """
        low level interface.
        skip the matrix data without reading it.
        """
        self.matrix_status = eMatrixDataSkipped
        cdef size_t bytes_read = SdifFSkipMatrixData(self.this)
        if self.matrix_idx == self.this.CurrFramH.NbMatrix:
            self.frame_status = eFrameAllDataRead
        return bytes_read
    
    def get_matrix_data(self):
        """
        read the data of the current matrix as a numpy array
        if the matrix-header was not read, it is read
        """
        if self.matrix_status == eMatrixNothingRead:
            self.read_matrix_header()
        self.read_matrix_data()
        return _array_from_matrix_data_no_copy(self.this.CurrMtrxData)
    
    def skip_matrix(self):
        """
        skip the matrix altogether. this is a low-level function
        only used when reading the data manually.
        If you are iterating with 
        for frame in sdiffile:
            for matrix in frame:
                ... etc ...
                
        then it is not necessary to take care of this because the
        SdifFile makes its own book-keeping and knows if something
        needs to be skipped or not
        """
        cdef size_t bytes_read = SdifFSkipMatrix(self.this)
        self.matrix_idx += 1
        if self.matrix_idx == self.this.CurrFramH.NbMatrix:
            self.frame_status = eFrameAllDataRead
        return bytes_read
    
    def status(self):
        """
        return (current-frame-status, current-matrix-index, current-matrix-status)
        """
        return self.frame_status, self.matrix_idx, self.matrix_status
    
    def __iter__(self): 
        return self
    def __next__(self):
        self.finalize_frame()
        if self.eof == 1:
            raise StopIteration
        self.frame_status = eFrameHeaderRead
        SdifFReadFrameHeader(self.this)
        return self.frame
    
    def get_next_matrix(self):
        """
        read the next matrix header and return a matrix with its
        data still not read. in particular, if the previous matrix
        was not read fully, its data is skipped
        """
        if self.matrix_idx == self.this.CurrFramH.NbMatrix:
            return None
        if self.matrix_status == eMatrixHeaderRead:
            SdifFSkipMatrixData(self.this)
        SdifFReadMatrixHeader(self.this)
        self.matrix_idx += 1
        self.matrix_status = eMatrixHeaderRead
        return self.matrix
    
    def rewind(self):
        """
        rewind the SdifFile. after this function is called, the file
        is in its starting frame (as if the file had been just open)
        """
        SdifFRewind(self.this)
        self.reinit()
        
    def _rewind(self):
        SdifFRewind(self.this)

    cdef void reinit(self):
        self.frame = None
        self.matrix_idx = 0
        self.matrix_status = eMatrixNothingRead
        SdifFReadGeneralHeader(self.this)
        SdifFReadAllASCIIChunks(self.this)
        #self.read_general_header()
        #self.read_all_ascii_chunks()
        self.eof = (self.this.CurrSignature == eEof)
        self.frame_status = eFrameSignatureRead

    def _read_padding(self):
        return SdifFReadPadding(self.this, SdifFPaddingCalculate(self.this.Stream, self.this.Pos))
        
    # .......................................................................
    # WRITING
    # 
    def add_NVT(self, dict d):
        """
        the NVT is a place to put metadata about the file
        it is a hash table: pairs of the sort (name: value)
        
        d is a python dictionary which is translated to a NVT
        """
        SdifNameValuesLNewTable(self.this.NameValues, _SdifNVTStreamID)
        for name, value in d.iteritems():
            value = str(value)
            SdifNameValuesLPutCurrNVT(self.this.NameValues, name, value)
            
    def add_matrix_type(self, str signature, column_names):
        """
        sdiff.add_matrix_type("1ABC", "Column1, Column2")
        or
        sdiff.add_matrix_type("1ABC", ["Column1", "Column2"])
        """
        if isinstance(column_names, basestring):
            column_names = column_names.split(",")
        cdef SdifMatrixTypeT *mt = MatrixType_create(signature, column_names)
        SdifPutMatrixType(self.this.MatrixTypesTable, mt)
        # TODO: is this a memory leak? who destroys the MatrixType?
        #self.frame_status = eFrameInvalid
        
    def add_frame_type(self, signature, components):
        """
        signature is a 4-char string
        components is a sequence of Component or just a sequence
        of tuples of the form (signature, name)
        
        alternatively it accepts the form defined in EaSDIF
        add_frame_type("1NEW", "1NEW NewMatrix, 1FQ0 New1FQ0")
        
        components are separated either by a ',' or by a ';'
        """
        if isinstance(components, basestring):
            if ',' in components:
                components = components.split(",")
            else: 
                components = components.split(';')
            components = [component.split() for component in components]
        cdef SdifFrameTypeT *ft = FrameType_create(signature, components)
        SdifPutFrameType(self.this.FrameTypesTable, ft)
        # TODO: is this a memory leak? who destroys the FrameType?
        #Mself.frame_status = eFrameInvalid
        
    def clone_type_definitions(self, SdifFile source):
        """
        clone the frame and matrix type definitions of source_sdiffile
        NB: this must be called only for writing mode before any frame 
        has been written
        """
        frametypes = source.get_frame_types()
        matrixtypes = source.get_matrix_types()
        for frametype in frametypes:
            self.add_frame_type(frametype.signature, frametype.components)
        for matrixtype in matrixtypes:
            self.add_matrix_type(matrixtype.signature, matrixtype.column_names)
        #self.write_all_ascii_chunks()  # this is not necessary since it will be called when the first frame is written
    
    def clone_NVTs(self, SdifFile source):
        """
        clone the NVT from source (an open SdifFile)
        
        NB: If you do not plan to midify the type definitions included
        in the source file, it's better to call 'clone_definitions', which
        clones everything but the data, so you can do
        new_sdif = SdifFile("out.sdif", "w")
        new_sdif.clone_definitions(old_sdif)
        for frame in old_sdif:
            new_frame = new_sdif.new_frame(frame.signature, frame.time)
            ... etc ...
        """
        for nvt in source.get_NVTs():
            self.add_NVT(nvt)
            
    def clone_definitions(self, SdifFile source):
        """
        clone both NVT(s) and frame and matrix definitions from source,
        so after calling this function you can start creating frames
        """
        self.clone_NVTs(source)
        self.clone_type_definitions(source)
    
    def clone_frames(self, SdifFile source, tuple signatures_to_clone=None):
        """
        clone all the frames in source which are included in 
        @signatures_to_clone -- if this is not defined, clone 
        everything
        
        NB: the use case for this function is when you want to
        modify some of the metadata but leave the data itself
        unmodified
        """
        # TODO : optimize this method
        if signatures_to_clone is not None:
            for frame0 in source:
                if frame0.signature in signatures_to_clone:
                    frame1 = self.new_frame(frame0.signature, frame0.time)
                    for matrix in frame0:
                        frame1.add_matrix(matrix.signature, matrix.get_data())
                    frame1.write()
        else:
            for frame0 in source:
                frame1 = self.new_frame(frame0.signature, frame0.time)
                for matrix in frame0:
                    frame1.add_matrix(matrix.signature, matrix.get_data())
                frame1.write()
            
    def add_streamID(self, unsigned int numid, char *source, char *treeway):
        """
        this method is only there for completion. it seems to be only used in old
        sdif types
        """
        SdifStreamIDTablePutSID(self.this.StreamIDsTable,
            numid, source, treeway)
            
    def add_predefined_type(self, predefined_type='RBEP'):
        """
        Add a predefined type to this SdifFile.
        This type must be already defined in the global
        variable SDIF_NEWTYPES.
        """
        frame_types, matrix_types = get_predefined_frames_and_matrices(predefined_type)
        if frame_types is not None:
            # type exists
            for frame_type in frame_types:
                self.add_frame_type(*frame_type)
            for matrix_type in matrix_types:
                self.add_matrix_type(*matrix_type)
        else:
            raise ValueError("Definition of sdif type not found")
            
    def write_all_ascii_chunks(self):
        """
        low level interface.
        Once the NVTs and matrix and frame definitions have been added to the SdifFile,
        this methods writes them all together to disk and the SdifFile is ready to accept
        new frames.
        """
        if self.write_status != eSdifGeneralHeader:
            return
        SdifFWriteAllASCIIChunks(self.this)
        self.write_status = eSdifAllASCIIChunks
        
    def new_frame(self, str signature, SdifFloat8 time, SdifUInt4 streamID=0):
        """
        create a new frame with the given signature and at the given time
        
        new_frame = sdiffile.new_frame('1SIG', time_now)
        new_frame.add_matrix(...)
        new_frame.write()
        
        if you know that you will write only one matrix, you can call
        
        sdiffile.new_frame_one_matrix(frame_sig, time_now, matrix_sig, data)
        
        and this will do the same as the three method calls above
        """
        # ask for a new frame means that we are through with
        # defining the global header (ascii chunks) so verify that
        # we have written it
        assert self.mode == eWriteFile or self.mode == eReadWriteFile
        if self.write_status == eSdifGeneralHeader:
            self.write_all_ascii_chunks()
        return FrameW_new(self, str2signature(signature), time, streamID)
        
    def new_frame_one_matrix(self, bytes frame_sig, SdifFloat8 time, bytes matrix_sig, c_numpy.ndarray data_array, SdifUInt4 streamID=0):
        """
        create a frame containing only one matrix and write it
        This method creates the frame, creates a new matrix
        in the frame and writes it to disk, all at once
        
        NB: use this method when you want to create a frame which
        contains only one matrix, like a 1TRC frame. It is more efficient
        than calling new_frame, add_matrix, write (see method 'new_frame')
        """
        assert self.mode == eWriteFile or self.mode == eReadWriteFile
        if self.write_status == eSdifGeneralHeader:
            self.write_all_ascii_chunks()
        cdef size_t frame_size = SdifSizeOfFrameHeader() + SdifSizeOfMatrix(
            <SdifDataTypeET>(dtype_numpy2sdif(data_array.descr.type_num)),
            data_array.shape[0], data_array.shape[1] # rows, cols
        )
        SdifFSetCurrFrameHeader(self.this, _str2sig(frame_sig), frame_size, 1, streamID, time)
        SdifFWriteFrameHeader(self.this)
        SdifFWriteMatrix(self.this, 
            _str2sig(matrix_sig), 
            <SdifDataTypeET>dtype_numpy2sdif(data_array.descr.type_num),
            data_array.shape[0], data_array.shape[1],   # rows, cols
            data_array.data)
        self.write_status = eSdifMatrixData
        
    # low level functions to print info
    def print_NVT(self): SdifFPrintAllNameValueNVT(self.this)
    def print_general_header(self): SdifFPrintGeneralHeader(self.this)
    def print_all_ascii_chunks(self): SdifFPrintAllASCIIChunks(self.this)
    def print_all_types(self): SdifFPrintAllType(self.this)
    def print_matrix_header(self): SdifFPrintMatrixHeader(self.this)
    def print_one_row(self): SdifFPrintOneRow(self.this)
    def print_frame_header(self): SdifFPrintFrameHeader(self.this)
    def print_all_stream_ID(self): SdifFPrintAllStreamID(self.this)
    
    def frame_types_to_string(self):
        """
        returns a string with all frame types
        """
        cdef SdifStringT *sdifstr
        sdifstr = SdifStringNew()
        SdifFAllFrameTypeToSdifString(self.this, sdifstr)
        out = PyString_from_SdifString(sdifstr)
        SdifStringFree(sdifstr)
        return out
        
    def get_frame_types(self):
        """
        returns a list of frame type definitions (1FTD)
        """
        return FrameTypesTable_to_list(self.this.FrameTypesTable)
        
    def matrix_types_to_string(self):
        """ returns a string with all matrix types"""
        cdef SdifStringT *sdifstr
        sdifstr = SdifStringNew()
        SdifFAllMatrixTypeToSdifString(self.this, sdifstr)
        out = PyString_from_SdifString(sdifstr)
        SdifStringFree(sdifstr)
        return out
        
    def get_matrix_types(self):
        """
        returns a list of matrix type definitions (1MTD)
        """
        return MatrixTypesTable_to_list(self.this.MatrixTypesTable)
        
    def get_NVTs(self):
        """
        return a list with all devined NameValueTables 
        each NVT is converted to a python dict
        """
        return valuetables_to_dicts (self.this.NameValues)
        
    def get_stream_IDs(self):
        return streamidtable_to_list(self.this.StreamIDsTable)
    
            
# -----------------------------------------------------------------------------------------------

def test_bench_read_sdif1(filename):
    sdif_file = SdifFile(filename)
    cdef int n
    while sdif_file.read_frame_header():
        for n in range(sdif_file.matrices_in_frame):
            sdif_file.get_matrix_data()
    sdif_file.close()
            
def test_bench_read_sdif2(filename):
    # this is the slowest version
    sdif_file = SdifFile(filename)
    for frame in sdif_file:
        for matrix in frame:
            matrix.get_data()
    sdif_file.close()
    
def test_bench_read_sdif6(filename):
    """
    this is pure c, has no overhead at all and thus is a good
    reference to measure the other implementations.
    at the moment, the best we get at the python 
    interpreter is 1.227 (22,7%) slower than pure c
    """
    cdef int n
    cdef size_t char_read 
    cdef SdifFileT *f = SdifFOpen(filename, eReadFile)
    SdifFReadGeneralHeader(f)
    SdifFReadAllASCIIChunks(f)
    cdef int eof = (f.CurrSignature == eEof)
    while eof == 0:
        SdifFReadFrameHeader(f)
        for n in range(f.CurrFramH.NbMatrix):
            SdifFReadMatrixHeader(f)
            SdifFReadMatrixData(f)
            # it is not totally pure c in the sense that at this
            # point we have a numpy array which we could use in 
            # python. 
            data = _array_from_matrix_data_no_copy(f.CurrMtrxData)
        eof = SdifFGetSignature(f, &char_read) == eEof
    SdifFClose(f)
    
def test_bench_read_sdif3(filename):
    # fastest, 4 is almost the same, 2 is twice as slow..., 
    # it should be different for frames which have
    # many matrices but that is really the exception.
    sdif_file = SdifFile(filename)
    cdef int n
    for frame in sdif_file:
        for n in range(len(frame)):
            sdif_file.get_next_matrix().get_data()
    sdif_file.close()

def test_bench_read_sdif4(filename):
    sdif_file = SdifFile(filename)
    cdef int n
    for frame in sdif_file:
        for n in range(len(frame)):
            frame.next().get_data()
    sdif_file.close()

def test_bench_read_sdif5(filename):
    # this is the fastest for common case where a frame has only one matrix
    # and you do not need to check the signature of each matrix 
    # (since the matrix-header and the data are read in one function-call)
    sdif_file = SdifFile(filename)
    for frame in sdif_file:
        frame.get_matrix_data() # assume that there is only one matrix per frame
    sdif_file.close()

def test_write(filename):
    s = SdifFile(filename, "w")
    s.add_matrix_type("1MTX", "ColumnaA")
    s.add_frame_type("1ABC", "1MTX pupu")
    s.add_NVT({"meta":"data"})
    m = numpy.array([0.0, 0.1, 0.3, 0.7, 0.4], dtype=float)
    m.resize((5, 1))
    frame = s.new_frame("1ABC", 0.0)
    frame.add_matrix("1MTX", m)
    frame.write()
