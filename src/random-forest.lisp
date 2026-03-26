(defpackage nndescent/random-forest
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rt #:nndescent/random-tree))
  (:export #:make-random-forest
           #:initial-approximation))
(in-package :nndescent/random-forest)

(serapeum:-> make-random-forest (list (integer 1) (integer 1))
             (values list &optional))
(defun make-random-forest (ps k n)
  (mapcar #'lparallel:force
          (loop repeat n
                collect (lparallel:future (rt:make-random-tree ps k)))))

(serapeum:-> initial-approximation (p:dist list (integer 1))
             (values list &optional))
(defun initial-approximation (dist ps k)
  (declare (optimize (speed 3)))
  (let ((forest (make-random-forest ps (floor (* k 1.5)) 10)))
    (flet ((make-queue (p)
             (let ((q (q:make-queue (1+ k))))
               (loop for tree in forest
                     for neighbors = (rt:neighbor-points tree p) do
                       (loop for %p in neighbors
                             for d = (funcall dist p %p)
                             unless (or (eq p %p)
                                        (q:in-queue-p %p q))
                               do (q:enqueue-limited! q %p (- d) k)))
               q)))
      (let ((qs (loop for p in ps
                      collect (let ((p p)) (lparallel:future (make-queue p))))))
        (mapcar #'lparallel:force qs)))))
