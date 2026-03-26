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

(defun gen-points (amount-generator point-generator)
  (lambda ()
    (loop repeat  (funcall amount-generator)
          collect (funcall point-generator))))

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
  (for-all ((points (gen-points
                     (gen-integer :min 2000 :max 10000)
                     (gen-point 400 1f0)))
            (conn   (gen-integer :min 10 :max 40)))
    (let ((tree (rt:make-random-tree points conn)))
      (loop for p in points
            for neighbors = (rt:neighbor-points tree p)
            do (is (member p neighbors :test #'eq))))))

(test approximation
  (for-all ((ps (gen-points
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    (let ((approx (rf:initial-approximation #'p:euclidean-dist ps 30))
          (exact  (n:knn #'p:euclidean-dist ps 30)))
      (is (< (loop for a in approx
                   for e in exact
                   count (< (diversion-index (q:to-sorted-list a) e) 15))
             (* (length ps) 0.1))))))

(in-suite nndescent)

;; This test is for me. It allows deeper understanding of what is
;; going on.
(test reverse-map
  (for-all ((ps (gen-points
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    (let* ((approx (nn:random-forest-approximation #'p:euclidean-dist ps 30))
           (reverse (nn:reverse-map approx)))
      (maphash
       (lambda (k v)
         (declare (ignore k))
         ;; Check there is no duplicates
         (is (equalp v (remove-duplicates v :test #'eq))))
       reverse))))

(test nndescent-improvement
  (for-all ((ps (gen-points
                 (gen-integer :min 1000 :max 5000)
                 (gen-point 3 1f0))))
    (let ((approx (nn:nndescent!
                   ps #'p:euclidean-dist
                   (nn:random-forest-approximation #'p:euclidean-dist ps 30)
                   30))
          (exact  (n:knn #'p:euclidean-dist ps 30)))
      (is (< (loop for a in approx
                   for e in exact
                   count (not (equalp a e)))
             (* (length ps) 0.02))))))
