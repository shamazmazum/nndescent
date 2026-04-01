(defpackage nndescent/random-tree
  (:use #:cl)
  (:shadow #:plusp)
  (:local-nicknames (#:a #:alexandria))
  (:export #:plusp
           #:node
           #:make-random-tree
           #:neighbor-points
           #:leaf-histogram
           #:depth-histogram))
(in-package :nndescent/random-tree)

;; Requires Euclidean vector space
;; (plusp p p1 p2) computes <p - (p1 + p2)/2, p1 - p2>
(deftype plusp ()
  '(function (t t t) (values boolean &optional)))
(deftype node () '(or node-inner node-leaf))

(serapeum:defconstructor node-leaf
  (points list))

(serapeum:defconstructor node-inner
  (p1    t)
  (p2    t)
  (plus  node)
  (minus node))

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

(serapeum:-> make-random-tree (plusp list a:positive-fixnum)
             (values node &optional))
(defun make-random-tree (plusp ps k)
  (declare (optimize (speed 3)))
  (let ((length (length ps)))
    (if (<= length k)
        (node-leaf ps)
        (multiple-value-bind (p1 p2)
            (random-elements ps length)
          (labels ((%go (plus minus xs)
                     (if (null xs)
                         (values plus minus)
                         (let ((p (car xs)))
                           (if (funcall plusp p p1 p2)
                               (%go (cons p plus) minus (cdr xs))
                               (%go plus (cons p minus) (cdr xs)))))))
            (multiple-value-bind (plus minus)
                (%go nil nil ps)
              (node-inner
               p1 p2
               (make-random-tree plusp plus  k)
               (make-random-tree plusp minus k))))))))

(serapeum:-> find-leaf (plusp node t)
             (values node-leaf &optional))
(defun find-leaf (plusp tree p)
  (declare (optimize (speed 3)))
  (if (leafp tree) tree
      (if (funcall plusp p (node-inner-p1 tree) (node-inner-p2 tree))
          (find-leaf plusp (node-inner-plus  tree) p)
          (find-leaf plusp (node-inner-minus tree) p))))

(serapeum:-> neighbor-points (plusp node t)
             (values list &optional))
(defun neighbor-points (plusp tree p)
  (declare (optimize (speed 3)))
  (node-leaf-points
   (find-leaf plusp tree p)))

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
                  (%go (node-inner-plus  node))
                  (%go (node-inner-minus node))))))
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
                     (%go (node-inner-plus  node) depth)
                     (%go (node-inner-minus node) depth)))))
      (%go tree 0))
    (sort
     (a:hash-table-alist table)
     #'< :key #'car)))
