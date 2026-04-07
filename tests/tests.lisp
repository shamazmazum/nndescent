(in-package :nndescent/tests)

(defun diversion-index (xs ys)
  (labels ((%go (n xs ys)
             (cond
               ((null xs) n)
               ((null ys) n)
               ((equalp
                 (car xs)
                 (car ys))
                (%go (1+ n) (cdr xs) (cdr ys)))
               (t n))))
    (%go 0 xs ys)))

(defun gen-point (n c)
  (lambda ()
    (make-array n
                :element-type 'single-float
                :initial-contents
                (loop repeat n collect (- (random (* 2 c)) c)))))

(defun gen-points/list (amount-generator point-generator)
  (lambda ()
    (loop repeat  (funcall amount-generator)
          collect (funcall point-generator))))

(defun gen-points/vector (amount-generator point-generator)
  (lambda ()
    (coerce
     (funcall (gen-points/list amount-generator point-generator))
     'vector)))

(defun run-tests ()
  (every #'identity
         (mapcar (lambda (suite)
                   (let ((status (run suite)))
                     (explain! status)
                     (results-status status)))
                 '(metrics random-tree nndescent))))

(defun run-tests-ci ()
  (setq *random-state* (make-random-state t)
        lparallel:*kernel* (lparallel:make-kernel 6))
  (run-tests))

(def-suite metrics     :description "Test metric functions")
(def-suite random-tree :description "Test random trees")
(def-suite nndescent   :description "Test nndescent algorithm")

;; Accurate metric functions
(serapeum:-> euclidean-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values single-float &optional))
(defun euclidean-dist (x y)
  (let ((state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (loop for i below (length x)
          do (setq state (vs:add state (expt (- (aref x i) (aref y i)) 2))))
    (sqrt (vs:state-sum state))))

(serapeum:-> manhattan-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values single-float &optional))
(defun manhattan-dist (x y)
  (let ((state (vs:sum-state 0.0)))
    (declare (dynamic-extent state))
    (loop for i below (length x)
          do (setq state (vs:add state (abs (- (aref x i) (aref y i))))))
    (vs:state-sum state)))

(serapeum:-> chebyshev-dist ((simple-array single-float (*))
                             (simple-array single-float (*)))
             (values single-float &optional))
(defun chebyshev-dist (x y)
  (loop for i below (length x)
        maximizing (abs (- (aref x i) (aref y i)))))

(in-suite metrics)

(test euclidean
  (for-all* ((d (gen-integer :min 2 :max 50))
             (x (gen-point d 1f0))
             (y (gen-point d 1f0)))
    (is-true
     (a:approxp (  euclidean-dist x y)
                (m:euclidean-dist x y)))))

(test manhattan
  (for-all* ((d (gen-integer :min 2 :max 50))
             (x (gen-point d 1f0))
             (y (gen-point d 1f0)))
    (is-true
     (a:approxp (  manhattan-dist x y)
                (m:manhattan-dist x y)))))

(test chebyshev
  (for-all* ((d (gen-integer :min 2 :max 50))
             (x (gen-point d 1f0))
             (y (gen-point d 1f0)))
    (is-true
     (a:approxp (  chebyshev-dist x y)
                (m:chebyshev-dist x y)))))

(in-suite random-tree)
(test neighbors
  (for-all ((points (gen-points/list
                     (gen-integer :min 2000 :max 10000)
                     (gen-point 400 1f0)))
            (conn   (gen-integer :min 10 :max 40))
            (dist   (gen-one-element #'m:euclidean-dist
                                     #'m:manhattan-dist
                                     #'m:chebyshev-dist)))
    (let ((tree (rt:make-random-tree dist points conn)))
      (is-true
       (every
        (lambda (p)
          (let ((neighbors (rt:neighbor-points dist tree p)))
            (member p neighbors :test #'eq)))
        points)))))

(test approximation
  (for-all ((ps (gen-points/vector
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    ;; FIXME: Fails with other metrics
    (let ((approx (rf:initial-approximation #'m:euclidean-dist ps 30))
          (exact  (n:knn-graph #'m:euclidean-dist ps 30)))
      (flet ((dequeue (q)
               (mapcar #'g:pgen-point
                       (q:to-sorted-list! q))))
        (is (< (loop for a across approx
                     for e across exact
                     count (< (diversion-index
                               (dequeue a)
                               (map 'list #'identity e))
                              15))
               (* (length ps) 0.1)))))))

(in-suite nndescent)

(test nndescent-improvement
  (for-all ((ps (gen-points/vector
                 (gen-integer :min 5000 :max 10000)
                 (gen-point 3 1f0)))
            (dist (gen-one-element #'m:euclidean-dist
                                   #'m:manhattan-dist
                                   #'m:chebyshev-dist)))
    (let ((approx (nn:nndescent!
                   dist ps (rf:initial-approximation dist ps 30) 30))
          (exact  (n:knn-graph dist ps 30)))
      (is (< (loop for a across approx
                   for e across exact
                   count (not (equalp a e)))
             (* (length ps) 0.02))))))

(test knn
  (for-all ((set (gen-points/vector
                  (gen-integer :min 5000 :max 10000)
                  (gen-point 3 1f0)))
            (qs  (gen-points/vector
                  (gen-integer :min 1000 :max 5000)
                  (gen-point 3 1f0)))
            (dist (gen-one-element #'m:euclidean-dist
                                   #'m:manhattan-dist
                                   #'m:chebyshev-dist)))
    (let* ((graph (nn:nndescent!
                   dist set (rf:initial-approximation dist set 30) 30))
           (approx (nn:knn dist set qs graph 2))
           (exact  (n:knn  dist set qs 2)))
      (is (< (loop for e across approx
                   for a across exact
                   count (not (equalp a e)))
             (* (length qs) 0.02))))))
