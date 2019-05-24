;;;; lexical-environment.lisp ---
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

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
