(defpackage nndescent/point
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:vs #:vector-sum))
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
  (let ((n (length x))
        (state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (assert (= n (length y)))
    ;; SETQ is for speed
    (loop for i below n do
          (setq state (vs:add state (expt (- (aref x i) (aref y i)) 2))))
    (sqrt (vs:state-sum state))))

(serapeum:-> manhattan-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun manhattan-dist (x y)
  "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)"
  (declare (optimize (speed 3)))
  (let ((n (length x))
        (state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (assert (= n (length y)))
    (loop for i below n do
          (setq state (vs:add state (abs (- (aref x i) (aref y i))))))
    (vs:state-sum state)))

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
