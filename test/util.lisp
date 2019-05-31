;;;; util.lisp --- Utilities for tests
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(defun make-empty-global-environment ()
  (let ((env (make-instance 'global-environment)))
    (setf (lookup 'function 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace)
          (lookup 'variable 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace))
    env))

(defun make-populated-global-environment ()
  (let ((env (make-instance 'global-environment)))
    (setf (lookup 'function 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace)
          (lookup 'variable 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace))
    (setf (lookup 'bar 'function env) :bar)
    env))
