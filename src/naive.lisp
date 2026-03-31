(defpackage nndescent/naive
  (:use #:cl)
  (:local-nicknames (#:p #:nndescent/point)
                    (#:q #:nndescent/pqueue))
  (:export #:knn-graph #:knn))
(in-package :nndescent/naive)

;; Here point sets could be lists, but I keep them simple-vectors for
;; consistency with nndescent.

(serapeum:-> knn-graph (p:dist simple-vector (integer 1))
             (values simple-vector &optional))
(defun knn-graph (dist ps k)
  (declare (optimize (speed 3)))
  (flet ((knn-list (p)
           (let ((q (q:make-queue (1+ k))))
             (loop for %p across ps
                   for d = (funcall dist p %p)
                   unless (eq p %p) do
                     (q:enqueue-limited! q %p (- d) k))
             (q:to-sorted-list q))))
    (let ((qs (loop for p across ps collect
                    (let ((p p))
                      (lparallel:future (knn-list p))))))
      (map 'vector #'lparallel:force qs))))

(serapeum:-> knn-single (p:dist simple-vector t (integer 1))
             (values list &optional))
(defun knn-single (dist ps p k)
  (declare (optimize (speed 3)))
  (let ((q (q:make-queue (1+ k))))
    (loop for %p across ps
          for d = (funcall dist p %p) do
            (q:enqueue-limited! q %p (- d) k))
    (q:to-sorted-list q)))

(serapeum:-> knn (p:dist simple-vector simple-vector (integer 1))
             (values simple-vector &optional))
(defun knn (dist ps queries k)
  (declare (optimize (speed 3)))
  (lparallel:pmap
   'vector
   (lambda (p)
     (knn-single dist ps p k))
   queries))
