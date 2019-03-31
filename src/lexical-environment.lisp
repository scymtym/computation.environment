(cl:in-package #:environment)

(defclass lexical-environment (meta-namespace-lookup-mixin
                               hierarchical-environment-mixin
                               bindings-mixin
                               print-items:print-items-mixin)
  ()
  (:default-initargs
   :parent (error "missing required initarg ~S" :parent)))

(defmethod initialize-instance :after ((instance lexical-environment)
                                       &key
                                       parent)
  (setf (%namespaces instance) (%namespaces parent)))
