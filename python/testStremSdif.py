import pysdif
import numpy as np

def makeFile():
    f = pysdif.SdifFile('testStreamSdif.sdif', 'w')
    f.add_streamID(0, 'a_source', 'a_tree')
    f.add_streamID(1, 'another_source', 'another_tree')
    f.add_frame_type('XFXX', 'XMXX anXMXX')
    f.add_matrix_type('XMXX', 'value')
    
    for n in range(100):
        if n < 50:
            q = 0
        else:
            q=1
        frame = f.new_frame('XFXX', n, q)
        for m in range(4):
            frame.add_matrix('XMXX', np.array([[float(m)]]))
        frame.write()
    f.close() 

def readFile():
    f = pysdif.SdifFile('testStreamSdif.sdif', 'r')
    for frame in f:
        print(frame.id)
        for matrix in frame:
            print(matrix.get_data())

def main():
    makeFile()
    readFile()

if __name__ == '__main__':
    main() 

