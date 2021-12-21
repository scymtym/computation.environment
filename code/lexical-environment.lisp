;;;; lexical-environment.lisp --- A lexical environment.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

(defclass lexical-environment (meta-namespace-lookup-mixin
                               bindings+hierarchical-environment-mixin
                               print-items:print-items-mixin)
  ()
  (:default-initargs
   :parent (error "missing required initarg ~S" :parent)))

(defmethod initialize-instance :after ((instance lexical-environment)
                                       &key parent)
  ;; Used the (shared) namespace lookup cache of PARENT.
  (setf (%namespaces instance) (%namespaces parent)))
