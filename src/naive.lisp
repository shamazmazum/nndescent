(defpackage nndescent/naive
  (:use #:cl)
  (:local-nicknames (#:p #:nndescent/point)
                    (#:q #:nndescent/pqueue))
  (:export #:knn))
(in-package :nndescent/naive)

(serapeum:-> knn (p:dist list (integer 1))
             (values list &optional))
(defun knn (dist ps k)
  (flet ((knn-list (p)
           (let ((q (q:make-queue (1+ k))))
             (loop for %p in ps
                   for d = (funcall dist p %p)
                   unless (eq p %p) do
                     (q:enqueue-limited! q %p (- d) k))
             (q:to-sorted-list q))))
    (let ((qs (loop for p in ps collect
                    (let ((p p))
                      (lparallel:future (knn-list p))))))
      (mapcar #'lparallel:force qs))))
