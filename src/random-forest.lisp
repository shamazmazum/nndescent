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

(serapeum:-> plusp (simple-vector p:plusp)
             (values p:plusp &optional))
(defun plusp (vector plusp)
  (declare (optimize (speed 3)))
  (lambda (i i1 i2)
    (funcall plusp
     (svref vector i)
     (svref vector i1)
     (svref vector i2))))

(serapeum:-> dist (simple-vector p:dist)
             (values p:dist &optional))
(defun dist (vector dist)
  (declare (optimize (speed 3)))
  (lambda (i1 i2)
    (funcall dist
             (svref vector i1)
             (svref vector i2))))

(serapeum:-> make-random-forest (simple-vector p:plusp a:positive-fixnum a:positive-fixnum)
             (values list &optional))
(defun make-random-forest (ps plusp k n)
  (declare (optimize (speed 3)))
  (let ((indices (loop for i below (length ps) collect i)))
    (mapcar #'lparallel:force
            (loop repeat n
                  collect
                  (lparallel:future
                    (rt:make-random-tree
                     (plusp ps plusp)
                     indices k))))))

(serapeum:-> initial-approximation (p:operations simple-vector a:positive-fixnum
                                    &optional a:positive-fixnum)
             (values simple-vector &optional))
(defun initial-approximation (ops ps k &optional (ntrees 10))
  (declare (optimize (speed 3)))
  (let ((forest (make-random-forest
                 ps (p:operations-plusp ops)
                 (floor (* k 1.5)) ntrees))
        (dist  (dist  ps (p:operations-dist  ops)))
        (plusp (plusp ps (p:operations-plusp ops))))
    (flet ((make-queue (idx)
             (let ((q (q:make-queue (1+ k))))
               (loop for tree in forest
                     for neighbors = (rt:neighbor-points plusp tree idx) do
                       (loop for %idx in neighbors
                             for d = (funcall dist idx %idx)
                             unless (or (= idx %idx)
                                        (q:in-queue-p q %idx :key #'g:pgen-point))
                               do (q:enqueue-limited! q (g:pgen %idx 0) (- d) k)))
               q)))
      (let ((qs (loop for i below (length ps)
                      collect (let ((i i)) (lparallel:future (make-queue i))))))
        (map 'vector #'lparallel:force qs)))))
