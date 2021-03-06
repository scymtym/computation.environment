;;;; conditions.lisp --- Conditions signaled by the computation.environment system.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

(define-condition environment-condition ()
  ((%environment :initarg :environment
                 :reader  environment)))

(define-condition namespace-does-not-exist-error (environment-condition)
  ((%name :initarg :name
          :reader  name))
  (:report (lambda (condition stream)
             (format stream "~@<Namespace ~S does not exist in ~
                             environment ~A~@:>"
                     (name        condition)
                     (environment condition)))))

(define-condition entry-does-not-exist-error (environment-condition
                                              error)
  ((%name      :initarg :name
               :reader  name)
   (%namespace :initarg :namespace
               :reader  namespace))
  (:report
   (lambda (condition stream)
     (let* ((environment    (environment condition))
            (namespace      (namespace   condition))
            (namespace-name (block nil
                              (map-entries (lambda (name value container)
                                             (declare (ignore container))
                                             (when (eq value namespace)
                                               (return name)))
                                           'namespace environment))))
       (format stream "~@<An entry for name ~S does not exist in ~
                      namespace ~A (~A) in environment ~A~@:>"
               (name condition) namespace-name namespace environment)))))
