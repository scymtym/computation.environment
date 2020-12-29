;;;; global-environment.lisp --- Tests for the global environment class.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.lexical-environment
  :in :computation.environment)

(test construction.smoke
  "Test constructing `global-environment' instances."

  (let ((env (make-instance 'env:global-environment)))
    (is (=     0   (entry-count 'function env)))
    (is (equal '() (entries 'function env)))))

(let ((env (make-instance 'env:global-environment)))
  (list (lookup 'env::namespace 'env::namespace env)
        (entries 'env::namespace env)))
