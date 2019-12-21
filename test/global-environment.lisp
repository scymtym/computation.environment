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

  (let ((env (make-instance 'global-environment)))
    (is (=     0   (entry-count 'function env)))
    (is (equal '() (entries 'function env)))))

(let ((env (make-instance 'global-environment)))
  (list (lookup 'computation.environment::namespace 'computation.environment::namespace env)
        (entries 'computation.environment::namespace env)))
