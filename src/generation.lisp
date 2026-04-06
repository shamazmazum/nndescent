(defpackage nndescent/generation
  (:use #:cl)
  (:export #:point #:generation #:iterations
           #:pgen #:pgen-point #:pgen-gen
           #:seen #:seen-point #:seen-flag))
(in-package :nndescent/generation)

;; Attaching generation tag to the point
(deftype point ()
  "Allowed index of a point in the vector"
  '(integer 0 #.(ash 1 (- 62 10))))

(deftype generation ()
  "Generation number"
  '(integer 0 1023))

(deftype iterations ()
  "Allowed number of iterations of the nndescent algo"
  '(integer 1 1024))

(serapeum:-> pgen (point generation)
             (values alexandria:non-negative-fixnum &optional))
(declaim (inline pgen))
(defun pgen (p gen)
  "Attach generation tag to a point index."
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


(serapeum:-> seen (point boolean)
             (values alexandria:non-negative-fixnum &optional))
(declaim (inline seen))
(defun seen (p flag)
  "Attach SEEN flag to a point."
  (logior (ash p 1) (if flag 1 0)))

(serapeum:-> seen-point (alexandria:non-negative-fixnum)
             (values point &optional))
(declaim (inline seen-point))
(defun seen-point (seen)
  (ash seen -1))

(serapeum:-> seen-flag (alexandria:non-negative-fixnum)
             (values boolean &optional))
(declaim (inline seen-flag))
(defun seen-flag (seen)
  (not (zerop (logand seen 1))))
