;;;; lexical-environment.lisp --- Tests for the lexical environment class.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.lexical-environment
  :in :computation.environment)

(test lexical-environment.construction.smoke
  "Test constructing `lexical-environment' instances."

  (signals error (make-instance 'lexical-environment))

  (let* ((parent (make-empty-global-environment))
         (env    (make-instance 'lexical-environment :parent parent)))
    (map-entries (lambda (name namespace)
                   (unless (eq name 'computation.environment::namespace)
                     (is (=     0   (entry-count namespace env)))
                     (is (equal '() (entries     namespace env)))))
                 'computation.environment::namespace env)))
