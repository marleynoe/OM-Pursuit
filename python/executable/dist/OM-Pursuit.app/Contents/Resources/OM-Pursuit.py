import os
import OMPursuit
import argparse
import scikits.audiolab as audiolab

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='OM-Pursuit')
    parser.add_argument('dictionary_path', help='Path to an OM-Pursuit dictionary SDIF file')
    parser.add_argument('constraint_path', help='Path to an OM-Pursuit constraint SDIF file')
    parser.add_argument('target_path', help='Path to a sound file')
    parser.add_argument('marker_path', help='Path to a marker SDIF file')
    parser.add_argument('mpconstraint_path', help='Path to an OM-Pursuit analysis mp-constraint SDIF file')
    parser.add_argument('output_path', help='Path to write model sound file')
    parser.add_argument('res_output_path', help='Path to write residual sound file')
    parser.add_argument('sdif_output_path', help='Path to write OM-Pursuit model SDIF file') 
    parser.add_argument('max_iterations', type=int, help='The maximum number of analysis iterations')
    parser.add_argument('--dsf', type=float, default=1, help='A positive float >= 1.  The analysis procedure will down-sample the requisite audio files by this factor, e.g. 44.1kHz files with a dsf argument of 2 will be processed at 22.05kHz')

    args = parser.parse_args()
    
    D = OMPursuit.OMPursuitDictionary(args.dictionary_path, args.dsf)
    Ac = OMPursuit.OMPAnalysisConstraintSet(args.mpconstraint_path)
    C = OMPursuit.OMPursuitCompoundConstraint(args.constraint_path)

    markers = OMPursuit.OMPursuitMarkers(args.marker_path)
    target = OMPursuit.OMPursuitTarget(args.target_path, args.dsf)
    target.zeropadSignal(D, markers)
                                                     
    A = OMPursuit.OMPursuitAnalysis(D, C, target, markers, Ac, args.max_iterations)
    A.constrainedMP()
    audiolab.aiffwrite(A.ompModel.signal, os.path.expanduser(args.output_path), A.ompModel.samplerate)
    audiolab.aiffwrite(A.ompTarget.signal, os.path.expanduser(args.res_output_path), A.ompModel.samplerate)

    A.writeModelSdif(args.sdif_output_path)
    
    

