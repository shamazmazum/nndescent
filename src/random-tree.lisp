(defpackage nndescent/random-tree
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:a #:alexandria))
  (:local-nicknames (#:p #:nndescent/point))
  (:export #:node
           #:make-random-tree
           #:neighbor-points
           #:leaf-histogram
           #:depth-histogram))
(in-package :nndescent/random-tree)

(deftype node () '(or node-inner node-leaf))

(serapeum:defconstructor node-leaf
  (points list))

(serapeum:defconstructor node-inner
  (p1 t)
  (p2 t)
  (s1 node)
  (s2 node))

(serapeum:-> leafp (node)
             (values boolean &optional))
(declaim (inline leafp))
(defun leafp (node)
  (typep node 'node-leaf))

(serapeum:-> distinct-randoms ((and (integer 2) a:array-length))
             (values a:array-length a:array-length &optional))
(defun distinct-randoms (n)
  (declare (optimize (speed 3)))
  (let ((k1 (random n))
        (k2 (random (1- n))))
    (values k1 (if (< k2 k1) k2 (1+ k2)))))

(serapeum:-> sorted-distinct-randoms ((and (integer 2) a:array-length))
             (values a:array-length a:array-length &optional))
(defun sorted-distinct-randoms (n)
  (declare (optimize (speed 3)))
  (multiple-value-bind (k1 k2)
      (distinct-randoms n)
    (if (< k1 k2)
        (values k1 k2)
        (values k2 k1))))

(serapeum:-> drop (list a:array-length)
             (values list &optional))
(defun drop (xs n)
  (declare (optimize (speed 3)))
  (if (zerop n) xs
      (drop (cdr xs) (1- n))))

(serapeum:-> random-elements (list &optional a:array-length)
             (values t t &optional))
(defun random-elements (xs &optional (length (length xs)))
  (declare (optimize (speed 3)))
  (assert (> length 1))
  (multiple-value-bind (k1 k2)
      (sorted-distinct-randoms length)
    (let ((xs (drop xs k1)))
      (values (car xs) (nth (- k2 k1) xs)))))

(serapeum:-> make-random-tree (p:dist list a:positive-fixnum)
             (values node &optional))
(defun make-random-tree (dist ps k)
  "Make a random tree from a list of elements @c(ps). The tree has a
property that close elements (accroding to the metric @c(dist)) do
likely end up in the same leaf of the tree. A leaf of the tree has no
more than @c(k) elements."
  (declare (optimize (speed 3)))
  (let ((length (length ps)))
    (if (<= length k)
        (node-leaf ps)
        (multiple-value-bind (p1 p2)
            (random-elements ps length)
          (labels ((%go (s1 s2 xs)
                     (if (null xs)
                         (values s1 s2)
                         (let ((p (car xs)))
                           (if (< (funcall dist p p1)
                                  (funcall dist p p2))
                               (%go (cons p s1) s2 (cdr xs))
                               (%go s1 (cons p s2) (cdr xs)))))))
            (multiple-value-bind (s1 s2)
                (%go nil nil ps)
              (node-inner
               p1 p2
               (make-random-tree dist s1 k)
               (make-random-tree dist s2 k))))))))

(serapeum:-> find-leaf (p:dist node t)
             (values node-leaf &optional))
(defun find-leaf (dist tree p)
  "Find a leaf of the tree which contains @c(p)."
  (declare (optimize (speed 3)))
  (if (leafp tree) tree
      (if (< (funcall dist p (node-inner-p1 tree))
             (funcall dist p (node-inner-p2 tree)))
          (find-leaf dist (node-inner-s1 tree) p)
          (find-leaf dist (node-inner-s2 tree) p))))

(serapeum:-> neighbor-points (p:dist node t)
             (values list &optional))
(defun neighbor-points (dist tree p)
  "Return a list of points which are close to @c(p)."
  (declare (optimize (speed 3)))
  (node-leaf-points
   (find-leaf dist tree p)))

(serapeum:-> leaf-histogram (node (integer 1))
             (values (simple-array a:non-negative-fixnum (*)) &optional))
(defun leaf-histogram (tree k)
  (let ((histogram (make-array
                    (1+ k)
                    :element-type 'a:non-negative-fixnum
                    :initial-element 0)))
    (labels ((%go (node)
               (cond
                 ((leafp node)
                  (incf (aref histogram (length (node-leaf-points node)))))
                 (t
                  (%go (node-inner-s1 node))
                  (%go (node-inner-s2 node))))))
      (%go tree))
    histogram))

(serapeum:-> depth-histogram (node)
             (values list &optional))
(defun depth-histogram (tree)
  (let ((table (make-hash-table)))
    (labels ((%go (node depth)
               (if (leafp node)
                   (incf (gethash depth table 0))
                   (let ((depth (1+ depth)))
                     (%go (node-inner-s1 node) depth)
                     (%go (node-inner-s2 node) depth)))))
      (%go tree 0))
    (sort
     (a:hash-table-alist table)
     #'< :key #'car)))
