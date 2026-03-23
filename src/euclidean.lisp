(defpackage nndescent/euclidean
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:p #:nndescent/point))
  (:export #:*euclidean-ops*))
(in-package :nndescent/euclidean)

(serapeum:-> plusp ((simple-array double-float (*))
                    (simple-array double-float (*))
                    (simple-array double-float (*)))
             (values boolean &optional))
(defun plusp (center direction point)
  (declare (optimize (speed 3)))
  (let ((n (length center)))
    (assert (= n (length direction) (length point)))
    (cl:plusp
     (loop for i below n
           sum (* (- (aref point i) (aref center i)) (aref direction i))
           double-float))))

(serapeum:-> op ((simple-array double-float (*))
                 (simple-array double-float (*))
                 real real)
             (values (simple-array double-float (*)) &optional))
(defun op (p1 p2 s1 s2)
  (declare (optimize (speed 3)))
  (let ((n (length p1)))
    (assert (= n (length p2)))
    (let ((res (make-array n :element-type 'double-float))
          (s1 (float s1 0d0))
          (s2 (float s2 0d0)))
      (loop for i below (length res) do
        (setf (aref res i)
              (+ (* s1 (aref p1 i))
                 (* s2 (aref p2 i)))))
      res)))

(serapeum:-> dist ((simple-array double-float (*))
                   (simple-array double-float (*)))
             (values (double-float 0d0) &optional))
(defun dist (p1 p2)
  (declare (optimize (speed 3)))
  (let ((n (length p1)))
    (assert (= n (length p2)))
    (sqrt
     (loop for i below n
           sum (expt (- (aref p1 i) (aref p2 i)) 2)
           double-float))))

(defparameter *euclidean-ops*
  (p:operations #'op #'plusp #'dist))
