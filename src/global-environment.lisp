;;;; global-environment.lisp --- A global environment implementation
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
  (let ((namespaces (%namespaces instance))
        #+no (bindings   (make-bindings (load-time-value
                                    (c2mop:class-prototype
                                     (c2mop:ensure-finalized
                                      (find-class 'eq-hash-table-bindings-mixin))))
                                   instance)))
    (setf (gethash **meta-namespace** (%bindings instance)) #+no bindings namespaces
          ;; (gethash 'namespace         bindings)             **meta-namespace**
          (gethash 'namespace         namespaces)           **meta-namespace**)))
