(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:a  #:alexandria)
                    (#:q  #:nndescent/pqueue)
                    (#:m  #:nndescent/metrics)
                    (#:g  #:nndescent/generation)
                    (#:rf #:nndescent/random-forest))
  (:export #:nndescent!
           #:nndescent
           #:knn))
(in-package :nndescent/nndescent)

(serapeum:-> reverse-map (simple-vector)
             (values simple-vector &optional))
(defun reverse-map (map)
  (declare (optimize (speed 3)))
  (let* ((length (length map))
         (result (make-array length :initial-element nil)))
    (loop for k below length
          for q = (svref map k) do
            (q:do-queue (v q)
              (push (g:pgen k (g:pgen-gen v))
                    (svref result (g:pgen-point v)))))
    result))

(serapeum:-> nndescent-update! (m:dist simple-vector
                                simple-vector
                                a:positive-fixnum
                                a:non-negative-fixnum)
             (values (integer 0) &optional))
(defun nndescent-update! (dist ps approx k gen)
  (declare (optimize (speed 3)))
  (let ((set (reverse-map approx))
        (dist (rf:dist ps dist))
        ;; Highly unlikely, this value can be bigger than most-positive-fixnum.
        updates)
    ;; This operation cannot be made in the main loop because k-NN
    ;; queues are altered during the process.
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
                                (if (enqueue! p2 p1 priority) 1 0))))
                         fixnum)
                   fixnum))
                 updates))))
    (reduce
     (lambda (updates future)
       (+ updates (lparallel:force future)))
     updates
     :initial-value 0)))

(serapeum:-> nndescent! (m:dist simple-vector simple-vector a:positive-fixnum &key
                         (:max-iterations g:iterations)
                         (:min-updates    a:non-negative-fixnum))
             (values simple-vector g:iterations (integer 0) &optional))
(defun nndescent! (dist ps approx k &key (max-iterations 5) (min-updates 0))
  "Inplace version of @c(nndescent)."
  (declare (optimize (speed 3)))
  (assert (= (length ps) (length approx)))
  (labels ((%go (gen updates)
             (if (zerop (- max-iterations gen))
                 (values gen updates)
                 (let ((updates (nndescent-update! dist ps approx k gen))
                       (next-gen (1+ gen)))
                   (if (<= updates min-updates)
                       (values next-gen updates)
                       (%go    next-gen updates))))))
    (multiple-value-bind (gens updates)
        (%go 0 0)
      (values
       (map 'vector
            (lambda (q)
              (q:to-sorted-list! q #'g:pgen-point))
            approx)
       gens updates))))

(serapeum:-> nndescent (m:dist simple-vector simple-vector a:positive-fixnum &key
                        (:max-iterations g:iterations)
                        (:min-updates    a:non-negative-fixnum))
             (values simple-vector g:iterations (integer 0) &optional))
(defun nndescent (dist ps approx k &key (max-iterations 5) (min-updates 0))
  "Return a k-NN connectivity graph of points in the set @c(ps)
according to the metric @c(dist). An initial approximation of the
result @c(approx) is required for this algorithm. Parameters
@c(max-iterations) and @c(min-updates) control the termination
condition of the algorithm, namely the algorithm terminates if the
number of updates of the graph is less than or equal to
@c(min-updates). Also return the number of iterations and the number
of graph updates at the last iterations."
  (nndescent! dist ps (map 'vector #'q:copy-queue approx) k
              :max-iterations max-iterations
              :min-updates    min-updates))

(serapeum:-> knn-single (m:dist simple-vector t simple-vector a:positive-fixnum)
             (values list &optional))
(defun knn-single (dist ps p graph k)
  (declare (optimize (speed 3)))
  (assert (= (length ps) (length graph)))
  (let ((q (q:make-queue (1+ k))))
    (labels ((%go! ()
               (let (bp bd)
                 ;; O(n) search here :(
                 (q:do-queue (%p q)
                   (unless (g:seen-flag %p)
                     (let ((d (funcall dist (svref ps (g:seen-point %p)) p)))
                       (when (or (null bp) (< d bd))
                         (setq bp %p bd d)))))
                 (when bp
                   (q:map-into!
                    q (lambda (%p)
                        (if (= (g:seen-point %p)
                               (g:seen-point bp))
                            (g:seen (g:seen-point %p) t) %p)))
                   (loop for idx in (svref graph (g:seen-point bp))
                         for d = (funcall dist (svref ps idx) p)
                         unless (q:in-queue-p q idx :key #'g:seen-point) do
                           (q:enqueue-limited! q (g:seen idx nil) (- d) k))
                   (%go!)))))
      (let* ((best-point (random (length ps)))
             (best-dist  (funcall dist (svref ps best-point) p)))
        (q:enqueue-limited! q (g:seen best-point nil) (- best-dist) k)
        (%go!)))
    (q:to-sorted-list!
     q (lambda (seen)
         (svref ps (g:seen-point seen))))))

(serapeum:-> knn (m:dist simple-vector simple-vector simple-vector a:positive-fixnum)
             (values simple-vector &optional))
(defun knn (dist ps queries graph k)
    "For each point \\(q\\) in the set @c(queries) find @c(k) points
from the set @c(ps) closest to \\(q\\) using an approximate algorithm
called nndescent. @c(graph) is l-NN connectivity graph for the points
@c(ps), \\(l > k\\). For \\(k = 1 \\dots 10\\) try \\(l = 30 \\dots
40\\)."
  (declare (optimize (speed 3)))
  (lparallel:pmap
   'vector
   (lambda (p)
     (knn-single dist ps p graph k))
   queries))
