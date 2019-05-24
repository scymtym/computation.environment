;;;; lexical-environment.lisp --- Tests for the lexical environment class.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.lexical-environment
  :in :computation.environment)

(test construction.smoke
  "Test constructing `lexical-environment' instances."

  (signals error (make-instance 'lexical-environment)))
