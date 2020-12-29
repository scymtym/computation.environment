;;;; lexical-environment.lisp --- Tests for the lexical environment class.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.lexical-environment
  :in :computation.environment)

(test lexical-environment.construction.smoke
  "Test constructing `lexical-environment' instances."

  (signals error (make-instance 'env:lexical-environment))

  (let* ((parent (make-empty-global-environment))
         (env    (make-instance 'env:lexical-environment :parent parent)))
    (env:map-entries (lambda (name namespace environment)
                       (unless (eq name 'namespace)
                         (is (=     0      (env:entry-count namespace env)))
                         (is (equal '()    (env:entries     namespace env)))
                         (is (eq    parent environment))))
                     'env::namespace env)))
