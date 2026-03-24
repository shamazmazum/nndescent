(defpackage nndescent/random-forest
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rt #:nndescent/random-tree))
  (:export #:make-random-forest
           #:initial-approximation))
(in-package :nndescent/random-forest)

(serapeum:-> make-random-forest (p:operations list (integer 1) (integer 1))
             (values list &optional))
(defun make-random-forest (ops ps k n)
  (mapcar #'lparallel:force
          (loop repeat n
                collect (lparallel:future (rt:make-random-tree ops ps k)))))

(serapeum:-> initial-approximation (p:operations list (integer 1))
             (values (simple-array q:queue (*)) &optional))
(defun initial-approximation (ops ps k)
  (declare (optimize (speed 3)))
  (let ((forest (make-random-forest ops ps (floor (* k 1.5)) 10)))
    (flet ((make-queue (p)
             (let ((q (q:make-queue (1+ k))))
               (loop for tree in forest
                     for neighbors = (rt:neighbor-points ops tree p) do
                       (loop for %p in neighbors
                             for dist = (p:dist ops p %p)
                             unless (or (eq p %p)
                                        (q:in-queue-p %p q :test #'eq))
                               do (q:enqueue-limited q %p (- dist) k)))
               q)))
      (let ((qs (loop for p in ps
                      collect (let ((p p)) (lparallel:future (make-queue p))))))
        (make-array (length ps)
                    :element-type 'q:queue
                    :initial-contents (mapcar #'lparallel:force qs))))))
