;;;; package.lisp --- Package definition for tests of the computation.environment system
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

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
