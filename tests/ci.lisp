(defun do-all()
  (handler-case
      (asdf:load-system :nndescent/tests)
    (error ()
      (uiop:quit 1)))
  (uiop:quit
   (if (uiop:call-function "nndescent/tests:run-tests-ci")
       0 1)))

(do-all)
