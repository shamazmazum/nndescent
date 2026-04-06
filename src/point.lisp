(defpackage nndescent/point
  (:use #:cl)
  (:shadow #:plusp)
  (:export #:dist
           #:euclidean-dist
           #:manhattan-dist
           #:chebyshev-dist))
(in-package :nndescent/point)

(deftype dist ()
  "Similarity metric type"
  '(function (t t)
    (values (real 0) &optional)))

;; Some commonly used metrics
(serapeum:-> euclidean-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun euclidean-dist (x y)
  "Calculate Euclidean distance for two vectors of single floats.

\\(\\rho(x, y) = \\sqrt{\\sum_k (x_k - y_k)^2}\\)"
  (declare (optimize (speed 3)))
  (let ((n (length x)))
    (assert (= n (length y)))
    (float
     (sqrt
      (loop for i below n
            sum
            (expt (- (float (aref x i) 0d0)
                     (float (aref y i) 0d0))
                  2)
            double-float))
     0f0)))

(serapeum:-> manhattan-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun manhattan-dist (x y)
  "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)"
  (declare (optimize (speed 3)))
  (let ((n (length x)))
    (assert (= n (length y)))
    (float
     (loop for i below n
           sum
           (abs (- (float (aref x i) 0d0)
                   (float (aref y i) 0d0)))
           double-float)
     0f0)))

(serapeum:-> chebyshev-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun chebyshev-dist (x y)
  "Calculate Chebyshev distance for two vectors of single floats.

\\(\\rho(x, y) = \\max_k \\lvert x_k - y_k \\rvert \\)"
  (declare (optimize (speed 3)))
  (let ((n (length x))
        (result 0f0))
    (assert (= n (length y)))
    (loop for i below n do
      (setq result (max result (abs (- (aref x i) (aref y i))))))
    result))
