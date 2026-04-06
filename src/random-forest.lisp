(defpackage nndescent/random-forest
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:a #:alexandria)
                    (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:g  #:nndescent/generation)
                    (#:rt #:nndescent/random-tree))
  (:export #:dist
           #:initial-approximation))
(in-package :nndescent/random-forest)

(serapeum:-> dist (simple-vector p:dist)
             (values p:dist &optional))
(defun dist (vector dist)
  (declare (optimize (speed 3)))
  (lambda (i1 i2)
    (funcall dist
             (svref vector i1)
             (svref vector i2))))

(serapeum:-> make-random-forest (simple-vector p:dist a:positive-fixnum a:positive-fixnum)
             (values list &optional))
(defun make-random-forest (ps dist k n)
  "Collect @c(n) trees in parallel."
  (declare (optimize (speed 3)))
  (let ((indices (loop for i below (length ps) collect i)))
    (mapcar #'lparallel:force
            (loop repeat n
                  collect
                  (lparallel:future
                    (rt:make-random-tree
                     (dist ps dist)
                     indices k))))))

(serapeum:-> initial-approximation (p:dist simple-vector a:positive-fixnum
                                    &optional a:positive-fixnum)
             (values simple-vector &optional))
(defun initial-approximation (dist ps k &optional (ntrees 10))
  "Make an initial approximation for k-NN connectivity graph of a
point set @c(ps). An optional parameter @c(ntrees) controls the number
of random trees used in the process (the bigger value means more
accurate result but is slower)."
  (declare (optimize (speed 3)))
  (let ((forest (make-random-forest ps dist (floor (* k 1.5)) ntrees))
        (dist (dist ps dist)))
    (flet ((make-queue (idx)
             (let ((q (q:make-queue (1+ k))))
               (loop for tree in forest
                     for neighbors = (rt:neighbor-points dist tree idx) do
                       (loop for %idx in neighbors
                             for d = (funcall dist idx %idx)
                             unless (or (= idx %idx)
                                        (q:in-queue-p q %idx :key #'g:pgen-point))
                               do (q:enqueue-limited! q (g:pgen %idx 0) (- d) k)))
               q)))
      (let ((qs (loop for i below (length ps)
                      collect (let ((i i)) (lparallel:future (make-queue i))))))
        (map 'vector #'lparallel:force qs)))))
