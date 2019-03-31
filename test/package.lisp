(cl:defpackage #:environment.test
  (:use
   #:cl

   #:fiveam)

  (:export
   #:run-tests))

(cl:in-package #:environment.test)

(def-suite :environment)

(defun run-tests ()
  (run! :environment))
