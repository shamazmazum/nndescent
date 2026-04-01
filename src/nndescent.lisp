(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:a  #:alexandria)
                    (#:q  #:nndescent/pqueue)
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
  (declare (optimize (speed 3)))
  (let* ((length (length map))
         (result (make-array length :initial-element nil)))
    (loop for k below length
          for q = (svref map k) do
            (q:do-queue (v q)
              (push (g:pgen k (g:pgen-gen v))
                    (svref result (g:pgen-point v)))))
    result))

(serapeum:-> nndescent-update! (p:dist simple-vector
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

(serapeum:-> nndescent! (p:dist simple-vector simple-vector a:positive-fixnum &key
                         (:max-iterations g:iterations)
                         (:min-updates    a:non-negative-fixnum))
             (values simple-vector &optional))
(defun nndescent! (dist ps approx k &key (max-iterations 5) (min-updates 0))
  (declare (optimize (speed 3)))
  (assert (= (length ps) (length approx)))
  (labels ((%go (gen)
             (if (zerop (- max-iterations gen)) approx
                 (let ((updates (nndescent-update! dist ps approx k gen)))
                   (if (<= updates min-updates) approx
                       (%go (1+ gen)))))))
    (%go 0))
  (map 'vector
       (lambda (q)
         (q:to-sorted-list q #'g:pgen-point))
       approx))

(serapeum:-> nndescent (p:dist simple-vector simple-vector a:positive-fixnum &key
                        (:max-iterations g:iterations)
                        (:min-updates    a:non-negative-fixnum))
             (values simple-vector &optional))
(defun nndescent (dist ps approx k &key (max-iterations 5) (min-updates 0))
  (nndescent! dist ps (map 'vector #'q:copy-queue approx) k
              :max-iterations max-iterations
              :min-updates    min-updates))

(serapeum:-> knn-single (p:dist simple-vector t simple-vector a:positive-fixnum)
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
    (q:to-sorted-list
     q (lambda (seen)
         (svref ps (g:seen-point seen))))))

(serapeum:-> knn (p:dist simple-vector simple-vector simple-vector a:positive-fixnum)
             (values simple-vector &optional))
(defun knn (dist ps queries graph k)
  (declare (optimize (speed 3)))
  (lparallel:pmap
   'vector
   (lambda (p)
     (knn-single dist ps p graph k))
   queries))
