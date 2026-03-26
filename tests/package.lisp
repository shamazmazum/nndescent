(defpackage nndescent/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:p  #:nndescent/point)
                    (#:q  #:nndescent/pqueue)
                    (#:rt #:nndescent/random-tree)
                    (#:rf #:nndescent/random-forest)
                    (#:n  #:nndescent/naive)
                    #+nil
                    (#:nn #:nndescent/nndescent))
  (:export #:run-tests))
