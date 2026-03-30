(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rf #:nndescent/random-forest))
  (:export #:nndescent!
           #:nndescent
           #:reverse-map)) ; for testing
(in-package :nndescent/nndescent)

(deftype point () '(integer 0 #.(ash 1 (- 62 10))))
(deftype generation () '(integer 0 1023))

(serapeum:-> pgen (point generation)
             (values alexandria:non-negative-fixnum &optional))
(declaim (inline pgen))
(defun pgen (p gen)
  (logior (ash p 10) gen))

(serapeum:-> pgen-point (alexandria:non-negative-fixnum)
             (values point &optional))
(declaim (inline pgen-point))
(defun pgen-point (pgen)
  (ash pgen -10))

(serapeum:-> pgen-gen (alexandria:non-negative-fixnum)
             (values generation &optional))
(declaim (inline pgen-gen))
(defun pgen-gen (pgen)
  (logand pgen 1023))

(serapeum:-> reverse-map (simple-vector)
             (values simple-vector &optional))
(defun reverse-map (map)
  (let* ((length (length map))
         (result (make-array length :initial-element nil)))
    (loop for k below length
          for q = (svref map k) do
            (q:do-queue (v q)
              (push (pgen k (pgen-gen v))
                    (svref result (pgen-point v)))))
    result))

(serapeum:-> attach-generation! (simple-vector)
             (values &optional))
(defun attach-generation! (approx)
  (loop for q across approx do
    (q:map-into!
     q (lambda (p)
         (pgen p 0))))
  (values))

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
                 (and (not (q:in-queue-p p2 q :key #'pgen-point))
                      (q:enqueue-limited! q (pgen p2 (1+ gen)) prio k))))))
      (loop for p below (length approx) do
        (let ((p p))
          (push
           (lparallel:future
             (loop for rest on (svref set p)
                   for p1 = (car rest)
                   sum
                   (loop for p2 in (cdr rest)
                         sum
                         (if (and (< (pgen-gen p1) gen)
                                  (< (pgen-gen p2) gen))
                             ;; Do not try to update two points from an older
                             ;; generation.
                             0
                             (let ((priority (- (funcall
                                                 dist
                                                 (pgen-point p1)
                                                 (pgen-point p2)))))
                               (+
                                ;; Check if p2 is a neighbor of p1
                                (if (enqueue!
                                     (pgen-point p1) (pgen-point p2) priority)
                                    1 0)
                                ;; And vice-versa
                                (if (enqueue!
                                     (pgen-point p2) (pgen-point p1) priority)
                                    1 0)))))))
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
  (attach-generation! approx)
  (labels ((%go (gen)
             (if (zerop (- max-iterations gen)) approx
                 (let ((updates (nndescent-update! dist ps approx k gen)))
                   (if (<= updates min-updates) approx
                       (%go (1+ gen)))))))
    (%go 0))
  (map 'vector
       (lambda (q)
         (mapcar
          (lambda (pgen)
            (aref ps (pgen-point pgen)))
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
