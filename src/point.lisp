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
                      (let ((p1 0f0)
                            (p2 0f0)
                            (p3 0f0)
                            (p4 0f0)
                            (p5 0f0)
                            (stop (logand n (lognot 3))))
                        (declare (type single-float p1 p2 p3 p4 p5))
                        (loop for i below stop by 4 do
                          (setq p1 (,accfn p1 (%go (+ i 0))))
                          (setq p2 (,accfn p2 (%go (+ i 1))))
                          (setq p3 (,accfn p3 (%go (+ i 2))))
                          (setq p4 (,accfn p4 (%go (+ i 3)))))
                        (loop for i from stop below n do
                          (setq p5 (,accfn p5 (%go i))))
                        (,finfn
                         (,accfn p1 p2 p3 p4 p5)))))))))
  (frob euclidean-dist + sqrt
        (expt (- (aref x idx)
                 (aref y idx))
              2)
        "Calculate Euclidean distance for two vectors of single floats.

\\(\ρ(x, y) = \\sqrt{\\sum_k (x_k - y_k)^2}\\)")

  (frob manhattan-dist + identity
        (abs (- (aref x idx)
                (aref y idx)))
        "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)")

  (frob chebyshev-dist max identity
        (abs (- (aref x idx)
                (aref y idx)))
        "Calculate Chebyshev distance for two vectors of single floats.

\\(\\rho(x, y) = \\max_k \\lvert x_k - y_k \\rvert \\)"))
