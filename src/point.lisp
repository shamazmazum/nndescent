(defpackage nndescent/point
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:vs #:vector-sum))
  (:export #:plusp #:dist
           #:operations #:operations-dist #:operations-plusp
           #:euclidean-dist
           #:manhattan-dist
           #:chebyshev-dist
           #:euclidean-plusp
           #:*euclidean-operations*))
(in-package :nndescent/point)

(deftype dist ()
  '(function (t t)
    (values (real 0) &optional)))

;; Requires Euclidean vector space
;; (plusp p p1 p2) computes <p - (p1 + p2)/2, p1 - p2>
(deftype plusp ()
  '(function (t t t)
    (values boolean &optional)))

(serapeum:-> euclidean-plusp ((simple-array single-float (*))
                              (simple-array single-float (*))
                              (simple-array single-float (*)))
             (values boolean &optional))
(defun euclidean-plusp (p p1 p2)
  (declare (optimize (speed 3)))
  (let ((n (length p))
        (state (vs:sum-state 0f0)))
    (declare (dynamic-extent state))
    (assert (= n (length p1) (length p2)))
    ;; SETQ is for speed
    (loop for i below n
          for %p  = (aref p  i)
          for %p1 = (aref p1 i)
          for %p2 = (aref p2 i) do
            (setq state
                  (vs:add state
                          (* (- %p (/ (+ %p1 %p2) 2))
                             (- %p1 %p2)))))
    (cl:plusp (vs:state-sum state))))

(serapeum:-> euclidean-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun euclidean-dist (p1 p2)
  (declare (optimize (speed 3)))
  (let ((n (length p1))
        (state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (assert (= n (length p2)))
    (loop for i below n do
          (setq state (vs:add state (expt (- (aref p1 i) (aref p2 i)) 2))))
    (sqrt (vs:state-sum state))))

(serapeum:defconstructor operations
  (dist  dist)
  (plusp plusp))

(defparameter *euclidean-operations*
  (operations #'euclidean-dist #'euclidean-plusp))

;; Some more commonly used metrics
(serapeum:-> manhattan-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun manhattan-dist (p1 p2)
  (declare (optimize (speed 3)))
  (let ((n (length p1))
        (state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (assert (= n (length p2)))
    (loop for i below n do
          (setq state (vs:add state (abs (- (aref p1 i) (aref p2 i))))))
    (vs:state-sum state)))

(serapeum:-> chebyshev-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values (single-float 0f0) &optional))
(defun chebyshev-dist (p1 p2)
  (declare (optimize (speed 3)))
  (let ((n (length p1))
        (result 0f0))
    (assert (= n (length p2)))
    (loop for i below n do
      (setq result (max result (abs (- (aref p1 i) (aref p2 i))))))
    result))
