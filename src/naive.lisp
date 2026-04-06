(defpackage nndescent/naive
  (:use #:cl)
  (:local-nicknames (#:a #:alexandria)
                    (#:p #:nndescent/point)
                    (#:q #:nndescent/pqueue))
  (:export #:knn-graph #:knn))
(in-package :nndescent/naive)

;; Here point sets could be lists, but I keep them simple-vectors for
;; consistency with nndescent.

(serapeum:-> knn-graph (p:dist simple-vector a:positive-fixnum)
             (values simple-vector &optional))
(defun knn-graph (dist ps k)
  "Make an exact k-NN connectivity graph of a point set @c(ps). This
is an exact and brute-force \\(O(n^2)\\) algorithm."
  (declare (optimize (speed 3)))
  (flet ((knn-list (p)
           (let ((q (q:make-queue (1+ k))))
             (loop for %p across ps
                   for idx fixnum from 0
                   for d = (funcall dist p %p)
                   unless (eq p %p) do
                     (q:enqueue-limited! q idx (- d) k))
             (q:to-sorted-list q))))
    (lparallel:pmap 'vector #'knn-list ps)))

(serapeum:-> knn-single (p:dist simple-vector t a:positive-fixnum)
             (values list &optional))
(defun knn-single (dist ps p k)
  (declare (optimize (speed 3)))
  (let ((q (q:make-queue (1+ k))))
    (loop for %p across ps
          for d = (funcall dist p %p) do
            (q:enqueue-limited! q %p (- d) k))
    (q:to-sorted-list q)))

(serapeum:-> knn (p:dist simple-vector simple-vector a:positive-fixnum)
             (values simple-vector &optional))
(defun knn (dist ps queries k)
  "For each point \\(q\\) in the set @c(queries) find @c(k) points
from the set @c(ps) closest to \\(q\\). This is an exact and
brute-force algorithm, \\(O(nm)\\) where \\(n\\) and \\(m\\) is a
number of points in each set."
  (declare (optimize (speed 3)))
  (lparallel:pmap
   'vector
   (lambda (p)
     (knn-single dist ps p k))
   queries))
