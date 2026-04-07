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
(macrolet ((frob (name accfn finfn expr docstring)
             `(progn
                (serapeum:-> ,name ((simple-array single-float (*))
                                    (simple-array single-float (*)))
                             (values (single-float 0f0) &optional))
                (defun ,name (x y)
                  ,docstring
                  (declare (optimize (speed 3) (sb-c:insert-array-bounds-checks 0)))
                  (flet ((%go (idx) ,expr))
                    (declare (inline %go))
                    (let ((n (length x)))
                      (assert (= n (length y)))
                      (let ((p1 0d0)
                            (p2 0d0)
                            (p3 0d0)
                            (stop (logand n (lognot 1))))
                        (declare (type double-float p1 p2 p3))
                        (loop for i below stop by 2 do
                          (setq p1 (,accfn p1 (%go (+ i 0))))
                          (setq p2 (,accfn p2 (%go (+ i 1)))))
                        (when (oddp n)
                          (setq p3 (%go (1- n))))
                        (float
                         (,finfn
                          (,accfn p1 p2 p3))
                         0f0))))))))
  (frob euclidean-dist + sqrt
        (expt (- (float (aref x idx) 0d0)
                 (float (aref y idx) 0d0))
              2)
        "Calculate Euclidean distance for two vectors of single floats.

\\(\ρ(x, y) = \\sqrt{\\sum_k (x_k - y_k)^2}\\)")

  (frob manhattan-dist + identity
        (abs (- (float (aref x idx) 0d0)
                (float (aref y idx) 0d0)))
        "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)")

  (frob chebyshev-dist max identity
        (abs (- (float (aref x idx) 0d0)
                (float (aref y idx) 0d0)))
        "Calculate Chebyshev distance for two vectors of single floats.

\\(\\rho(x, y) = \\max_k \\lvert x_k - y_k \\rvert \\)"))
