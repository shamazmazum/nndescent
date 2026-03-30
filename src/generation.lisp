(defpackage nndescent/generation
  (:use #:cl)
  (:export #:point #:generation
           #:pgen #:pgen-point #:pgen-gen))
(in-package :nndescent/generation)

;; Attaching generation tag to the point
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
