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
                 '(random-tree nndescent))))

(def-suite random-tree :description "Test random trees")
(def-suite nndescent   :description "Test nndescent algorithm")

(in-suite random-tree)
(test neighbors
  (for-all ((points (gen-points/list
                     (gen-integer :min 2000 :max 10000)
                     (gen-point 400 1f0)))
            (conn   (gen-integer :min 10 :max 40)))
    (let ((tree (rt:make-random-tree #'p:plusp points conn)))
      (loop for p in points
            for neighbors = (rt:neighbor-points #'p:plusp tree p)
            do (is (member p neighbors :test #'eq))))))

(test approximation
  (for-all ((ps (gen-points/vector
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    (let ((approx (rf:initial-approximation #'p:euclidean-dist ps 30))
          (exact  (n:knn-graph #'p:euclidean-dist ps 30)))
      (flet ((dequeue (q)
               (mapcar #'g:pgen-point
                       (q:to-sorted-list q))))
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
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    (let ((approx (nn:nndescent!
                   #'p:euclidean-dist
                   ps (rf:initial-approximation #'p:euclidean-dist ps 30) 30))
          (exact  (n:knn-graph #'p:euclidean-dist ps 30)))
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
                  (gen-point 3 1f0))))
    (let* ((graph (nn:nndescent!
                   #'p:euclidean-dist
                   set (rf:initial-approximation #'p:euclidean-dist set 30) 30))
           (approx (nn:knn #'p:euclidean-dist set qs graph 2))
           (exact  (n:knn  #'p:euclidean-dist set qs 2)))
      (is (< (loop for e across approx
                   for a across exact
                   count (not (equalp a e)))
             (* (length qs) 0.02))))))
