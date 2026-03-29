(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rf #:nndescent/random-forest))
  (:export #:nndescent!
           #:nndescent
           #:reverse-map)) ; for testing
(in-package :nndescent/nndescent)

(serapeum:-> reverse-map (simple-vector)
             (values simple-vector &optional))
(defun reverse-map (map)
  (let* ((length (length map))
         (result (make-array length :initial-element nil)))
    (loop for k below length
          for q = (svref map k) do
            (q:do-queue (v q)
              (push k (svref result v))))
    result))

(serapeum:-> nndescent-update! (p:dist simple-vector simple-vector (integer 1))
             (values (integer 0) &optional))
(defun nndescent-update! (dist ps approx k)
  (let ((reverse (reverse-map approx))
        (dist (rf:dist ps dist))
        updates)
    (flet ((enqueue! (p1 p2 prio)
             (let ((q (svref approx p1)))
               ;; In some (quite rare, hopefully) cases, two or more
               ;; threads can write to the same queue. Just guard it
               ;; with a lock (but maybe there are better ways to
               ;; avoid the race.
               (q:with-queue-lock (q)
                 (and (not (q:in-queue-p p2 q))
                      (q:enqueue-limited! q p2 prio k))))))
      (loop for p below (length approx)
            for q = (svref approx p) do
              (let ((p p)
                    (q q))
                (push
                 (lparallel:future
                   (let ((set (svref reverse p)))
                     (q:do-queue (v q)
                       (pushnew v set :test #'eq))
                     (loop for rest on set
                           for p1 = (car rest)
                           sum
                           (loop for p2 in (cdr rest)
                                 for priority = (- (funcall dist p1 p2))
                                 sum
                                 (+
                                  ;; Check if p2 is a neighbor of p1
                                  (if (enqueue! p1 p2 priority) 1 0)
                                  ;; And vice-versa
                                  (if (enqueue! p2 p1 priority) 1 0))))))
                 updates))))
    (reduce
     (lambda (updates future)
       (+ updates (lparallel:force future)))
     updates
     :initial-value 0)))

(serapeum:-> nndescent! (simple-vector simple-vector p:dist (integer 1) &key
                         (:max-iterations (integer 1))
                         (:min-updates    (integer 0)))
             (values simple-vector &optional))
(defun nndescent! (ps approx dist k &key (max-iterations 5) (min-updates 0))
  (labels ((%go (n)
             (if (zerop n) approx
                 (let ((updates (nndescent-update! dist ps approx k)))
                   (if (<= updates min-updates) approx
                       (%go (1- n)))))))
    (%go max-iterations))
  (map 'vector
       (lambda (q)
         (mapcar
          (lambda (idx)
            (aref ps idx))
          (q:to-sorted-list q)))
       approx))

(serapeum:-> nndescent (simple-vector simple-vector p:dist (integer 1) &key
                        (:max-iterations (integer 1))
                        (:min-updates    (integer 0)))
             (values simple-vector &optional))
(defun nndescent (ps approx dist k &key (max-iterations 5) (min-updates 0))
  (nndescent! ps (map 'vector #'q:copy-queue approx) dist k
              :max-iterations max-iterations
              :min-updates    min-updates))
