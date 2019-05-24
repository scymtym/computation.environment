;;;; global-environment.lisp --- Tests for the global environment class.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.lexical-environment
  :in :computation.environment)

(test construction.smoke
  "Test constructing `global-environment' instances."

  (finishes (make-instance 'global-environment)))
