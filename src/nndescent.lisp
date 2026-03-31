(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:g  #:nndescent/generation)
                    (#:rf #:nndescent/random-forest))
  (:export #:nndescent!
           #:nndescent
           #:knn))
(in-package :nndescent/nndescent)

(serapeum:-> reverse-map (simple-vector)
             (values simple-vector &optional))
(defun reverse-map (map)
  (let* ((length (length map))
         (result (make-array length :initial-element nil)))
    (loop for k below length
          for q = (svref map k) do
            (q:do-queue (v q)
              (push (g:pgen k (g:pgen-gen v))
                    (svref result (g:pgen-point v)))))
    result))

(serapeum:-> nndescent-update! (p:dist simple-vector simple-vector
                                (integer 1) alexandria:non-negative-fixnum)
             (values (integer 0) &optional))
(defun nndescent-update! (dist ps approx k gen)
  (let ((set (reverse-map approx))
        (dist (rf:dist ps dist))
        updates)
    (loop for p below (length approx)
          for q = (svref approx p) do
            (q:do-queue (v q)
              (pushnew v (svref set p) :test #'eq)))
    (flet ((enqueue! (p1 p2 prio)
             (let ((q (svref approx p1)))
               ;; In some (quite rare, hopefully) cases, two or more
               ;; threads can write to the same queue. Just guard it
               ;; with a lock (but maybe there are better ways to
               ;; avoid the race.
               (q:with-queue-lock (q)
                 (and (not (q:in-queue-p q p2 :key #'g:pgen-point))
                      (q:enqueue-limited! q (g:pgen p2 (1+ gen)) prio k))))))
      (loop for p below (length approx) do
        (let ((p p))
          (push
           (lparallel:future
             (loop for rest on (svref set p)
                   for p1 = (car rest)
                   sum
                   (loop for p2 in (cdr rest)
                         sum
                         (if (and (< (g:pgen-gen p1) gen)
                                  (< (g:pgen-gen p2) gen))
                             ;; Do not try to update two points from an older
                             ;; generation.
                             0
                             (let* ((p1 (g:pgen-point p1))
                                    (p2 (g:pgen-point p2))
                                    (priority (- (funcall dist p1 p2))))
                               (+
                                ;; Check if p2 is a neighbor of p1
                                (if (enqueue! p1 p2 priority) 1 0)
                                ;; And vice-versa
                                (if (enqueue! p2 p1 priority) 1 0)))))))
                 updates))))
    (reduce
     (lambda (updates future)
       (+ updates (lparallel:force future)))
     updates
     :initial-value 0)))

(serapeum:-> nndescent! (p:dist simple-vector simple-vector (integer 1) &key
                         (:max-iterations (integer 1))
                         (:min-updates    (integer 0)))
             (values simple-vector &optional))
(defun nndescent! (dist ps approx k &key (max-iterations 5) (min-updates 0))
  (labels ((%go (gen)
             (if (zerop (- max-iterations gen)) approx
                 (let ((updates (nndescent-update! dist ps approx k gen)))
                   (if (<= updates min-updates) approx
                       (%go (1+ gen)))))))
    (%go 0))
  (map 'vector
       (lambda (q)
         (q:to-sorted-list q :key #'g:pgen-point))
       approx))

(serapeum:-> nndescent (p:dist simple-vector simple-vector (integer 1) &key
                        (:max-iterations (integer 1))
                        (:min-updates    (integer 0)))
             (values simple-vector &optional))
(defun nndescent (dist ps approx k &key (max-iterations 5) (min-updates 0))
  (assert (= (length ps) (length approx)))
  (nndescent! dist ps (map 'vector #'q:copy-queue approx) k
              :max-iterations max-iterations
              :min-updates    min-updates))

(serapeum:-> knn-single (p:dist simple-vector t simple-vector (integer 1))
             (values list &optional))
(defun knn-single (dist ps p graph k)
  (assert (= (length ps) (length graph)))
  (let ((q (q:make-queue (1+ k))))
    (labels ((%go! (best-point best-dist)
               (let ((bp best-point)
                     (bd best-dist))
                 (loop for idx in (svref graph best-point)
                       for d = (funcall dist (svref ps idx) p)
                       for %p = (svref ps idx)
                       unless (q:in-queue-p q %p) do
                         (q:enqueue-limited! q %p (- d) k)
                         (when (< d best-dist)
                           (setq bp idx
                                 bd d)))
                 (when (/= bp best-point)
                   (%go! bp bd)))))
      (let* ((best-point (random (length ps)))
             (best-dist  (funcall dist (svref ps best-point) p)))
      (%go! best-point best-dist)))
    (q:to-sorted-list q)))

(serapeum:-> knn (p:dist simple-vector simple-vector simple-vector (integer 1))
             (values simple-vector &optional))
(defun knn (dist ps queries graph k)
  (declare (optimize (speed 3)))
  (lparallel:pmap
   'vector
   (lambda (p)
     (knn-single dist ps p graph k))
   queries))
