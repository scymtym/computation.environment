(cl:defpackage #:computation.environment.test
  (:use
   #:cl

   #:fiveam

   #:computation.environment)

  (:export
   #:run-tests))

(cl:in-package #:computation.environment.test)

(def-suite :computation.environment)

(defun run-tests ()
  (run! :computation.environment))
