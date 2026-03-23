(defpackage nndescent/tests
  (:use #:cl #:fiveam)
  (:local-nicknames (#:e  #:nndescent/euclidean)
                    (#:rt #:nndescent/random-tree))
  (:export #:run-tests))
