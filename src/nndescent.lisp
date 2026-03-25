(defpackage nndescent/nndescent
  (:use #:cl)
  (:local-nicknames (#:q  #:nndescent/pqueue)
                    (#:p  #:nndescent/point)
                    (#:rf #:nndescent/random-forest))
  (:export #:random-forest-approximation
           #:reverse-map)) ; for testing
(in-package :nndescent/nndescent)

;; FIXME: It would be better if rf:initial-approximation returned a
;; vector with indices into an array of points.
(serapeum:-> random-forest-approximation (p:operations list (integer 1))
             (values hash-table &optional))
(defun random-forest-approximation (ops ps k)
  (let ((qs (rf:initial-approximation ops ps k))
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

(serapeum:-> join-sets (list list)
             (values list &optional))
(defun join-sets (s1 s2)
  (declare (optimize (speed 3)))
  (let (s)
    (loop for x in s1 do (pushnew x s :test #'eq))
    (loop for x in s2 do (pushnew x s :test #'eq))
    s))

(serapeum:-> nndescent-update! (p:dist hash-table (integer 1))
             (values (integer 0) &optional))
(defun nndescent-update! (dist approx k)
  (let ((reverse (reverse-map approx))
        (updates 0))
    (flet ((enqueue! (p1 p2 prio)
             (let ((q (gethash p1 approx)))
               (when (and (not (q:in-queue-p p2 q :test #'eq))
                          (q:enqueue-limited! q p2 prio k))
                 (incf updates)))))
      (maphash
       (lambda (p q)
         (let* ((forward (q:to-list q))
                (reverse (gethash p reverse))
                (ps (join-sets forward reverse)))
           (loop for rest on ps
                 for p1 = (car rest) do
                   (loop for p2 in (cdr rest)
                         for priority = (- (funcall dist p1 p2)) do
                           ;; Check if p2 is a neighbor of p1
                           (enqueue! p1 p2 priority)
                           ;; And vice-versa
                           (enqueue! p2 p1 priority)))))
       approx))
    updates))
