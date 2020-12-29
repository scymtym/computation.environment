;;;; util.lisp --- Utilities for tests
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

;;; Comparison

(defun set-equal/equal (set1 set2)
  (a:set-equal set1 set2 :test #'equal))

;;; Prepared environments

(defun make-empty-global-environment ()
  (let ((env (make-instance 'global-environment)))
    (setf (lookup 'function 'env::namespace env)
          (make-instance 'env::eq-namespace)
          (lookup 'variable 'env::namespace env)
          (make-instance 'env::eq-namespace))
    env))

(defun make-populated-global-environment ()
  (let ((env (make-instance 'global-environment)))
    (setf (env:lookup 'function 'env::namespace env)
          (make-instance 'env::eq-namespace)
          (env:lookup 'variable 'env::namespace env)
          (make-instance 'env::eq-namespace))
    (setf (env:lookup 'bar 'function env) :bar
          (env:lookup 'baz 'function env) :baz
          (env:lookup 'fez 'function env) :fez)
    env))

(defun make-populated-lexical-environment ()
  (let* ((parent (make-populated-global-environment))
         (env    (make-instance 'env:lexical-environment :parent parent)))
    (setf (env:lookup 'baz 'function env) :baz2          ; overwrite
          (env:lookup 'fez 'function env) env::+unbound+ ; undefined
          (env:lookup 'woo 'function env) :woo)          ; augment
    env))
