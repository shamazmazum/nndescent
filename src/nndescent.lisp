(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rf #:nndescent/random-forest))
  (:export #:random-forest-approximation
           #:reverse-map)) ; for testing
(in-package :nndescent/nndescent)

;; FIXME: It would be better if rf:initial-approximation returned a
;; vector with indices into an array of points.
(serapeum:-> random-forest-approximation (p:operations list (integer 1))
             (values hash-table &optional))
(defun random-forest-approximation (ops ps k)
  (let ((qs (rf:initial-approximation ops ps k))
        (approx (make-hash-table :test #'eq)))
    (loop for p in ps
          for q in qs do
            (setf (gethash p approx) q))
    approx))

(serapeum:-> reverse-map (hash-table)
             (values hash-table &optional))
(defun reverse-map (map)
  (let ((result (make-hash-table :test #'eq)))
    (maphash
     (lambda (k q)
       (q:do-queue (v q)
         (push k (gethash v result nil))))
     map)
    result))
