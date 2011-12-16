(in-package :om)

(defmethod! get-bpf-points ((self bpf-lib))
            (let ((bpflist (bpf-list self)))
                    (loop for bpf in bpflist collect
                    (point-pairs bpf)
                    ))
              )
                  
