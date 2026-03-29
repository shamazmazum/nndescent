;; Written by Michał "phoe" Herda, MIT licensed
;; https://github.com/phoe/damn-fast-priority-queue

(defpackage #:nndescent/pqueue
  (:use #:cl)
  (:shadow #:map)
  (:local-nicknames (#:a #:alexandria))
  (:export #:queue #:make-queue #:copy-queue
           #:enqueue! #:enqueue-limited! #:in-queue-p
           #:dequeue! #:peek #:size #:trim! #:map #:do-queue
           #:to-list #:to-sorted-list #:with-queue-lock
           #:queue-size-limit-reached
           #:queue-size-limit-reached-queue #:queue-size-limit-reached-object))
(in-package #:nndescent/pqueue)

(deftype data-type () 't)
(deftype data-vector-type () '(simple-array data-type (*)))
(deftype prio-type () 'single-float)
(deftype prio-vector-type () '(simple-array prio-type (*)))
(deftype extension-factor-type () '(integer 2 256))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Structure definition

(defstruct (queue (:conc-name #:%) (:constructor %make)
                  (:predicate nil) (:copier nil))
  (data-vector (make-array 256 :element-type 'data-type) :type data-vector-type)
  (prio-vector (make-array 256 :element-type 'prio-type) :type prio-vector-type)
  (size 0 :type a:array-length)
  (extension-factor 2 :type extension-factor-type)
  (extend-queue-p t :type boolean)
  (locked nil :type boolean))

(serapeum:-> make-queue (&optional a:array-index extension-factor-type boolean)
             (values queue &optional))
(defun make-queue (&optional
                     (initial-storage-size 256)
                     (extension-factor 2)
                     (extend-queue-p t))
  (%make :extension-factor extension-factor
         :data-vector (make-array initial-storage-size
                                  :element-type 'data-type)
         :prio-vector (make-array initial-storage-size
                                  :element-type 'prio-type)
         :extend-queue-p extend-queue-p))

(defmethod print-object ((object queue) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "(~D)" (%size object))))

(serapeum:-> copy-queue (queue) (values queue &optional))
(defun copy-queue (queue)
  (%make :extension-factor (%extension-factor queue)
         :size (%size queue)
         :extend-queue-p (%extend-queue-p queue)
         :data-vector (copy-seq (%data-vector queue))
         :prio-vector (copy-seq (%prio-vector queue))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; A version of adjust-array that returns simple-arrays
;;;; with all Common Lisp implementations. Implemented as a macro
;;;; to avoid introducing runtime overhead.

;;;; There are two branches:
;;;;  - adjust-array: works with SBCL and ABCL, fails with
;;;;    ECL and Clasp because the returned array is not simple
;;;;  - make-array plus loop for copying: works with all compilers

;;;; Performance notes:
;;;;  - With ABCL, adjust-array is slightly faster than making a new array
;;;;  - With SBCL, both versions have equal performance.
;;;;  - Using cl:replace instead of an explicit loop does not improve
;;;;    performance on any tested implementation.

(defmacro adjust-array* (array new-length)
  ;; Implementations that return a simple-array.
  #+(or abcl sbcl)
  `(adjust-array ,array ,new-length)
  ;; Implementations that may return a non-simple array.
  ;; Note: this code assumes a simple-vector as input.
  #-(or abcl sbcl)
  (a:with-gensyms (array* new-length* length* new-array* i*)
    `(let* ((,array* ,array)
            (,new-length* ,new-length)
            (,length* (array-total-size ,array*))
            (,new-array* (make-array ,new-length*
                                     :element-type (array-element-type ,array*))))
       (dotimes (,i* (min ,length* ,new-length*))
         (setf (row-major-aref ,new-array* ,i*)
               (row-major-aref ,array* ,i*)))
       ,new-array*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Dequeueing

(serapeum:-> heapify-downwards! (data-vector-type prio-vector-type a:array-index)
             (values &optional))
(declaim (inline heapify-downwards!))
(defun heapify-downwards! (data-vector prio-vector size)
  (let ((parent-index 0))
    (loop
      (let* ((left-index (+ (* parent-index 2) 1))
             (left-index-validp (< left-index size))
             (right-index (+ (* parent-index 2) 2))
             (right-index-validp (< right-index size)))
        (flet ((swap-left ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector left-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector left-index))
                 (setf parent-index left-index))
               (swap-right ()
                 (rotatef (aref prio-vector parent-index)
                          (aref prio-vector right-index))
                 (rotatef (aref data-vector parent-index)
                          (aref data-vector right-index))
                 (setf parent-index right-index)))
          (declare (inline swap-left swap-right))
          (when (and (not left-index-validp)
                     (not right-index-validp))
            (return))
          (when (and left-index-validp
                     (< (aref prio-vector parent-index)
                        (aref prio-vector left-index))
                     (or (not right-index-validp)
                         (< (aref prio-vector parent-index)
                            (aref prio-vector right-index))))
            (return))
          (if (and right-index-validp
                   (<= (aref prio-vector right-index)
                       (aref prio-vector left-index)))
              (swap-right)
              (swap-left))))))
  (values))

(serapeum:-> dequeue! (queue) (values t boolean &optional))
(defun dequeue! (queue)
  (declare (optimize (speed 3)))
  (if (zerop (%size queue))
      (values nil nil)
      (let ((data-vector (%data-vector queue))
            (prio-vector (%prio-vector queue)))
        (multiple-value-prog1 (values (aref data-vector 0) t)
          (decf (%size queue))
          (let ((old-data (aref data-vector (%size queue)))
                (old-prio (aref prio-vector (%size queue))))
            (setf (aref data-vector 0) old-data
                  (aref prio-vector 0) old-prio))
          (heapify-downwards! data-vector prio-vector (%size queue))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Enqueueing

(serapeum:-> heapify-upwards! (data-vector-type prio-vector-type a:array-length)
             (values &optional))
(declaim (inline heapify-upwards!))
(defun heapify-upwards! (data-vector prio-vector index)
  (do ((child-index index parent-index)
       (parent-index (ash (1- index) -1) (ash (1- parent-index) -1)))
      ((zerop child-index))
    (let ((child-priority (aref prio-vector child-index))
          (parent-priority (aref prio-vector parent-index)))
      (cond ((< child-priority parent-priority)
             (rotatef (aref prio-vector parent-index)
                      (aref prio-vector child-index))
             (rotatef (aref data-vector parent-index)
                      (aref data-vector child-index)))
            (t (return)))))
  (values))

(serapeum:-> enqueue! (queue t prio-type) (values &optional))
(defun enqueue! (queue object priority)
  (declare (optimize (speed 3)))
  (symbol-macrolet ((data-vector (%data-vector queue))
                    (prio-vector (%prio-vector queue)))
    (let ((size (%size queue))
          (extension-factor (%extension-factor queue))
          (length (array-total-size data-vector)))
      (when (>= size length)
        (unless (%extend-queue-p queue)
          (error 'queue-size-limit-reached :queue queue :element object))
        (let ((new-length (max 1 (mod (* length extension-factor)
                                      (ash 1 64)))))
          (declare (type a:array-length new-length))
          (when (<= new-length length)
            (error "Integer overflow while resizing array: new-length ~D is ~
                    smaller than old length ~D" new-length length))
          (setf data-vector (adjust-array* data-vector new-length)
                prio-vector (adjust-array* prio-vector new-length))))
      (setf (aref data-vector size) object
            (aref prio-vector size) priority)
      (heapify-upwards! data-vector prio-vector (%size queue))
      (incf (%size queue))
      (values))))

(serapeum:-> enqueue-limited! (queue t prio-type a:array-length)
             (values boolean &optional))
(defun enqueue-limited! (queue object priority limit)
  (cond
    ((< (%size queue) limit)
     (enqueue! queue object priority)
     t)
    ((> priority (aref (%prio-vector queue) 0))
     (enqueue! queue object priority)
     (dequeue! queue)
     t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Introspection and maintenance

(defmacro do-queue ((object queue &optional result) &body body)
  (multiple-value-bind (forms declarations) (a:parse-body body)
    (a:with-gensyms (i)
      (a:once-only (queue)
        `(dotimes (,i (%size ,queue) ,result)
           (let ((,object (aref (%data-vector ,queue) ,i)))
             ,@declarations
             (tagbody ,@forms)))))))

(serapeum:-> peek (queue) (values t boolean &optional))
(declaim (inline peek))
(defun peek (queue)
  (if (zerop (%size queue))
      (values nil nil)
      (values (aref (%data-vector queue) 0) t)))

(serapeum:-> size (queue) (values a:array-length &optional))
(declaim (inline size))
(defun size (queue)
  (%size queue))

(serapeum:-> trim! (queue) (values &optional))
(declaim (inline trim!))
(defun trim! (queue)
  (let ((size (%size queue)))
    (setf (%data-vector queue) (adjust-array* (%data-vector queue) size)
          (%prio-vector queue) (adjust-array* (%prio-vector queue) size)))
  (values))

(serapeum:-> in-queue-p (t queue)
             (values t &optional))
(defun in-queue-p (object queue)
  (declare (optimize (speed 3) (space 0)))
  (find object (%data-vector queue) :test #'eq :end (%size queue)))

(serapeum:-> map (queue (serapeum:-> (t) t)) (values &optional))
(defun map (queue function)
  (dotimes (i (%size queue))
    (funcall function (aref (%data-vector queue) i)))
  (values))

(serapeum:-> to-sorted-list (queue)
             (values list &optional))
(defun to-sorted-list (q)
  (let ((q (copy-queue q)) acc)
    (loop for obj = (dequeue! q)
          while obj do (push obj acc))
    acc))

(serapeum:-> to-list (queue)
             (values list &optional))
(defun to-list (q)
  (let (acc)
    (do-queue (x q)
      (push x acc))
    acc))

(defun acquire-lock (queue)
  (loop
    (unless (sb-ext:compare-and-swap (%locked queue) nil t)
      (return t))))

(defun release-lock (lock)
  (setf (%locked lock) nil))

(defmacro with-queue-lock ((queue) &body body)
  (let ((sym (gensym)))
    `(let ((,sym ,queue))
       (acquire-lock ,sym)
       (unwind-protect
            (progn ,@body)
         (release-lock ,sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Conditions

(define-condition queue-size-limit-reached (error)
  ((%queue :reader queue-size-limit-reached-queue :initarg :queue)
   (%object :reader queue-size-limit-reached-object :initarg :element))
  (:default-initargs :queue (a:required-argument :queue)
                     :element (a:required-argument :element))
  (:report
   (lambda (condition stream)
     (let ((queue (queue-size-limit-reached-queue condition))
           (element (queue-size-limit-reached-object condition)))
       (format stream "Size limit (~D) reached for non-extensible ~
                    queue ~S while trying to enqueue element ~S onto it."
               (length (%data-vector queue)) queue element)))))
