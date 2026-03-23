(defpackage nndescent/random-forest
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rt #:nndescent/random-tree))
  (:export #:make-random-forest))
(in-package :nndescent/random-forest)

;; TODO: parallelize
(serapeum:-> make-random-forest (p:operations list (integer 1) (integer 1))
             (values list &optional))
(defun make-random-forest (ops ps k n)
  (mapcar #'lparallel:force
          (loop repeat n
                collect (lparallel:future (rt:make-random-tree ops ps k)))))
