(cl:in-package #:environment)

(defclass global-environment (meta-namespace-lookup-mixin
                              bindings-mixin
                              print-items:print-items-mixin)
  ((%namespaces :initform (make-hash-table :test #'eq))))

(defmethod initialize-instance :after ((instance global-environment) &key)
  (let ((namespaces (%namespaces instance)))
    (setf (gethash **meta-namespace** (%bindings instance))
          (list (lambda (name)
                  (gethash name namespaces))
                (lambda (new-value name)
                  (setf (gethash name namespaces) new-value))
                (lambda ()
                  (hash-table-alist namespaces)))
          (gethash 'namespace namespaces)
          **meta-namespace**)))
