;;;; .lisp ---
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

(defclass global-environment (meta-namespace-lookup-mixin
                              bindings-mixin
                              print-items:print-items-mixin)
  ((%namespaces :initform (make-hash-table :test #'eq))))

(defmethod initialize-instance :after ((instance global-environment) &key)
  (let ((namespaces (%namespaces instance)))
    (setf (gethash **meta-namespace** (%bindings instance))
          (make-bindings (load-time-value (c2mop:class-prototype
                                           (c2mop:ensure-finalized
                                            (find-class 'eq-hash-table-bindings-mixin))))
                         instance)
          (gethash 'namespace namespaces)
          **meta-namespace**)))
