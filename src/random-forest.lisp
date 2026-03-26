(defpackage nndescent/random-forest
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rt #:nndescent/random-tree))
  (:export #:make-random-forest
           #:initial-approximation))
(in-package :nndescent/random-forest)

(serapeum:-> plusp (simple-vector rt:plusp)
             (values rt:plusp &optional))
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

(serapeum:-> make-random-forest (simple-vector (integer 1) (integer 1))
             (values list &optional))
(defun make-random-forest (ps k n)
  (let ((indices (loop for i below (length ps) collect i)))
    (mapcar #'lparallel:force
            (loop repeat n
                  collect
                  (lparallel:future
                    (rt:make-random-tree
                     (plusp ps #'p:plusp)
                     indices k))))))

(serapeum:-> initial-approximation (p:dist simple-vector (integer 1))
             (values simple-vector &optional))
(defun initial-approximation (dist ps k)
  (declare (optimize (speed 3)))
  (let ((forest (make-random-forest ps (floor (* k 1.5)) 10))
        (dist (dist ps dist))
        (plusp (plusp ps #'p:plusp)))
    (flet ((make-queue (idx)
             (let ((q (q:make-queue (1+ k))))
               (loop for tree in forest
                     for neighbors = (rt:neighbor-points plusp tree idx) do
                       (loop for %idx in neighbors
                             for d = (funcall dist idx %idx)
                             unless (or (= idx %idx)
                                        (q:in-queue-p %idx q))
                               do (q:enqueue-limited! q %idx (- d) k)))
               q)))
      (let ((qs (loop for i below (length ps)
                      collect (let ((i i)) (lparallel:future (make-queue i))))))
        (map 'vector #'lparallel:force qs)))))
