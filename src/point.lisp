(defpackage nndescent/point
  (:use #:cl)
  (:shadow #:plusp)
  (:export #:operations
           #:op #:plusp #:dist))
(in-package :nndescent/point)

(deftype dist  () '(serapeum:-> (t t)           (values (real 0) &optional)))
(deftype op    () '(serapeum:-> (t t real real) (values t        &optional)))
(deftype plusp () '(serapeum:-> (t t t)         (values boolean  &optional)))

(serapeum:defconstructor operations
  (op    op)
  (plusp plusp)
  (dist  dist))

(serapeum:-> plusp (operations t t t)
             (values boolean &optional))
(declaim (inline plusp))
(defun plusp (ops center direction point)
  (funcall (operations-plusp ops) center direction point))

(serapeum:-> op (operations t t real real)
             (values t &optional))
(declaim (inline op))
(defun op (ops p1 p2 s1 s2)
  (funcall (operations-op ops) p1 p2 s1 s2))

(serapeum:-> dist (operations t t)
             (values (real 0) &optional))
(declaim (inline dist))
(defun dist (ops p1 p2)
  (funcall (operations-dist ops) p1 p2))
