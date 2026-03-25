(defsystem :nndescent
  :name :nndescent
  :version "0.1"
  :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
  :description "Approximate algorithm for solving k-NN problem"
  :licence "2-clause BSD"
  :pathname "src"
  :serial t
  :class :package-inferred-system
  :depends-on (:alexandria
               :serapeum
               :lparallel
               :nndescent/point
               :nndescent/euclidean
               :nndescent/random-tree
               :nndescent/pqueue
               :nndescent/random-forest
               :nndescent/naive
               :nndescent/nndescent)
  :in-order-to ((test-op (load-op "nndescent/tests")))
  :perform (test-op (op system)
                      (declare (ignore op system))
                      (uiop:symbol-call :nndescent/tests '#:run-tests)))

(defsystem :nndescent/tests
    :name :nndescent/tests
    :author "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :license "2-clause BSD"
    :serial t
    :pathname "tests"
    :components ((:file "package")
                 (:file "tests"))
    :depends-on (:fiveam :nndescent))
