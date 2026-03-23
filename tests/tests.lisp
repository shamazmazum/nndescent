(in-package :nndescent/tests)

(defun gen-point (n c)
  (lambda ()
    (make-array n
                :element-type 'double-float
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
                 '(random-tree))))

(def-suite random-tree :description "Test random trees")

(in-suite random-tree)
(test neighbors
  (for-all ((points (gen-points
                     (gen-integer :min 2000 :max 10000)
                     (gen-point 400 1d0)))
            (conn   (gen-integer :min 10 :max 40)))
    (let ((tree (rt:make-random-tree e:*euclidean-ops* points conn)))
      (loop for p in points
            for neighbors = (rt:neighbor-points e:*euclidean-ops* tree p)
            do (is (member p neighbors :test #'eq))))))
