;;;; util.lisp --- Utilities for tests
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

;;; Comparison

(defun set-equal/equal (set1 set2)
  (alexandria:set-equal set1 set2 :test #'equal))

;;; Prepared environments

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
    (setf (lookup 'bar 'function env) :bar
          (lookup 'baz 'function env) :baz
          (lookup 'fez 'function env) :fez)
    env))

(defun make-populated-lexical-environment ()
  (let* ((parent (make-populated-global-environment))
         (env    (make-instance 'lexical-environment :parent parent)))
    (setf (lookup 'baz 'function env) :baz2          ; overwrite
          (lookup 'fez 'function env) computation.environment::+unbound+ ; undefined
          (lookup 'woo 'function env) :woo)          ; augment
    env))
