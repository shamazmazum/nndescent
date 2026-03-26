(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rf #:nndescent/random-forest))
  (:export #:random-forest-approximation
           #:nndescent!
           #:reverse-map)) ; for testing
(in-package :nndescent/nndescent)

;; FIXME: It would be better if rf:initial-approximation returned a
;; vector with indices into an array of points.
(serapeum:-> random-forest-approximation (p:dist list (integer 1))
             (values hash-table &optional))
(defun random-forest-approximation (dist ps k)
  (let ((qs (rf:initial-approximation dist ps k))
        (approx (make-hash-table :test #'eq)))
    (loop for p in ps
          for q in qs do
            (setf (gethash p approx) q))
    approx))

(serapeum:-> reverse-map (hash-table)
             (values hash-table &optional))
(defun reverse-map (map)
  (let ((result (make-hash-table :test #'eq)))
    (maphash
     (lambda (k q)
       (q:do-queue (v q)
         (push k (gethash v result nil))))
     map)
    result))

(serapeum:-> join! (list list)
             (values list &optional))
(defun join! (s1 s2)
  (declare (optimize (speed 3)))
  (loop for x in s2 do (pushnew x s1 :test #'eq))
  s1)

(serapeum:-> nndescent-update! (p:dist hash-table (integer 1))
             (values (integer 0) &optional))
(defun nndescent-update! (dist approx k)
  (let ((reverse (reverse-map approx))
        updates)
    (flet ((enqueue! (p1 p2 prio)
             (let ((q (gethash p1 approx)))
               (and (not (q:in-queue-p p2 q))
                    (q:enqueue-limited! q p2 prio k)))))
      (maphash
       (lambda (p q)
         (push
          (lparallel:future
            (let* ((forward (q:to-list q))
                   (reverse (gethash p reverse))
                   (ps (join! forward reverse)))
              (loop for rest on ps
                    for p1 = (car rest) sum
                      (loop for p2 in (cdr rest)
                            for priority = (- (funcall dist p1 p2)) sum
                              (+
                               ;; Check if p2 is a neighbor of p1
                               (if (enqueue! p1 p2 priority) 1 0)
                               ;; And vice-versa
                               (if (enqueue! p2 p1 priority) 1 0))))))
          updates))
       approx))
    (reduce
     (lambda (updates future)
       (+ updates (lparallel:force future)))
     updates
     :initial-value 0)))

(serapeum:-> nndescent! (list p:dist hash-table (integer 1) &key
                         (:max-iterations (integer 1))
                         (:min-updates    (integer 0)))
             (values list &optional))
(defun nndescent! (ps dist approx k &key (max-iterations 5) (min-updates 0))
  (labels ((%go (n)
             (if (zerop n) approx
                 (let ((updates (nndescent-update! dist approx k)))
                   (if (<= updates min-updates) approx
                       (%go (1- n)))))))
    (%go max-iterations))
  (mapcar
   (lambda (p)
     (q:to-sorted-list
      (gethash p approx)))
   ps))
