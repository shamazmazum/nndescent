(defpackage nndescent/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:e  #:nndescent/euclidean)
                    (#:q  #:nndescent/pqueue)
                    (#:rt #:nndescent/random-tree)
                    (#:rf #:nndescent/random-forest)
                    (#:n  #:nndescent/naive)
                    (#:nn #:nndescent/nndescent))
  (:export #:run-tests))
