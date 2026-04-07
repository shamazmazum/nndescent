(defpackage nndescent/metrics
  ;; For backward compatibility
  (:nicknames #:nndescent/point)
  (:use #:cl)
  (:shadow #:plusp)
  (:export #:dist
           #:euclidean-dist
           #:manhattan-dist
           #:chebyshev-dist))
(in-package :nndescent/metrics)

(deftype dist ()
  "Similarity metric type"
  '(function (t t)
    (values (single-float 0.0) &optional)))

;; Some commonly used metrics

;; SSE versions
#+(and sbcl x86-64)
(progn
  (serapeum:-> euclidean-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun euclidean-dist (x y)
    "Calculate Euclidean distance for two vectors of single floats.

\\(\ρ(x, y) = \\sqrt{\\sum_k (x_k - y_k)^2}\\)"
    (declare (optimize (speed 3) (sb-c:insert-array-bounds-checks 0)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (let ((p1 (sb-simd-sse:make-f32.4 0f0 0f0 0f0 0f0))
            (p2 0f0)
            (stop (logand n (lognot 3))))
        (loop for i below stop by 4 do
          (let* ((x (sb-simd-sse:f32.4-aref x i))
                 (y (sb-simd-sse:f32.4-aref y i))
                 (d (sb-simd-sse:f32.4- x y))
                 (p (sb-simd-sse:f32.4* d d)))
            (setq p1 (sb-simd-sse:f32.4+ p1 p))))
        (loop for i from stop below n do
          (setq p2 (+ p2 (expt (- (aref x i) (aref y i)) 2))))
        (sqrt
         (+ (sb-simd-sse:f32.4-horizontal+ p1) p2)))))

  (serapeum:-> manhattan-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun manhattan-dist (x y)
    "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)"
    (declare (optimize (speed 3) (sb-c:insert-array-bounds-checks 0)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (let ((p1 (sb-simd-sse:make-f32.4 0f0 0f0 0f0 0f0))
            (mask (let ((mask (sb-kernel:make-single-float #x7fffffff)))
                    (sb-simd-sse:make-f32.4 mask mask mask mask)))
            (p2 0f0)
            (stop (logand n (lognot 3))))
        (loop for i below stop by 4 do
          (let* ((x (sb-simd-sse:f32.4-aref x i))
                 (y (sb-simd-sse:f32.4-aref y i))
                 (d (sb-simd-sse:f32.4- x y))
                 (p (sb-simd-sse:f32.4-and d mask)))
            (setq p1 (sb-simd-sse:f32.4+ p1 p))))
        (loop for i from stop below n do
          (setq p2 (+ p2 (abs (- (aref x i) (aref y i))))))
        (+ (sb-simd-sse:f32.4-horizontal+ p1) p2))))

  (serapeum:-> chebyshev-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun chebyshev-dist (x y)
    "Calculate Chebyshev distance for two vectors of single floats.

\\(\\rho(x, y) = \\max_k \\lvert x_k - y_k \\rvert \\)"
    (declare (optimize (speed 3) (sb-c:insert-array-bounds-checks 0)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (let ((p1 (sb-simd-sse:make-f32.4 0f0 0f0 0f0 0f0))
            (mask (let ((mask (sb-kernel:make-single-float #x7fffffff)))
                    (sb-simd-sse:make-f32.4 mask mask mask mask)))
            (p2 0f0)
            (stop (logand n (lognot 3))))
        (loop for i below stop by 4 do
          (let* ((x (sb-simd-sse:f32.4-aref x i))
                 (y (sb-simd-sse:f32.4-aref y i))
                 (d (sb-simd-sse:f32.4- x y))
                 (p (sb-simd-sse:f32.4-and d mask)))
            (setq p1 (sb-simd-sse:f32.4-max p1 p))))
        (loop for i from stop below n do
          (setq p2 (max p2 (abs (- (aref x i) (aref y i))))))
        (max (sb-simd-sse:f32.4-horizontal-max p1) p2)))))

;; Slow versions without SSE
#-(and sbcl x86-64)
(progn
  (serapeum:-> euclidean-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun euclidean-dist (x y)
    "Calculate Euclidean distance for two vectors of single floats.

\\(\ρ(x, y) = \\sqrt{\\sum_k (x_k - y_k)^2}\\)"
    (declare (optimize (speed 3)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (sqrt
       (loop for i below n
             sum (expt (- (aref x i) (aref y i)) 2)))))

  (serapeum:-> manhattan-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun manhattan-dist (x y)
    "Calculate Manhattan distance for two vectors of single floats.

\\(\\rho(x, y) = \\sum_k \\lvert x_k - y_k \\rvert \\)"
    (declare (optimize (speed 3)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (loop for i below n
            sum (abs (- (aref x i) (aref y i))))))

  (serapeum:-> chebyshev-dist ((simple-array single-float (*))
                               (simple-array single-float (*)))
               (values (single-float 0f0) &optional))
  (defun chebyshev-dist (x y)
    "Calculate Chebyshev distance for two vectors of single floats.

\\(\\rho(x, y) = \\max_k \\lvert x_k - y_k \\rvert \\)"
    (declare (optimize (speed 3)))
    (let ((n (length x)))
      (assert (= n (length y)))
      (loop for i below n
            maximize (abs (- (aref x i) (aref y i)))))))
