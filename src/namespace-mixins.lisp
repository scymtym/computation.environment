;;;; namespace-mixins.lisp --- Namespace mixin classes.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

;;; `hash-table-bindings-mixin'

(defclass hash-table-bindings-mixin ()
  ())

(defmethod entry-count-in-bindings ((bindings    hash-table)
                                    (namespace   hash-table-bindings-mixin)
                                    (environment t))
  (hash-table-count bindings))

(defmethod map-entries-in-bindings ((function    function)
                                    (bindings    hash-table)
                                    (namespace   hash-table-bindings-mixin)
                                    (environment t))
  (maphash function bindings))

(defmethod entries-in-bindings ((bindings    hash-table) ; TODO remove this?
                                (namespace   hash-table-bindings-mixin)
                                (environment t))
  (hash-table-alist bindings))

(defmethod lookup-in-bindings ((name        t)
                               (bindings    hash-table)
                               (namespace   hash-table-bindings-mixin)
                               (environment t))
  (gethash name bindings))

(defmethod (setf lookup-in-bindings) ((new-value   t)
                                      (name        t)
                                      (bindings    hash-table)
                                      (namespace   hash-table-bindings-mixin)
                                      (environment t))
  (setf (gethash name bindings) new-value))

;;; `equal-hash-table-bindings-mixin'

(defclass equal-hash-table-bindings-mixin (hash-table-bindings-mixin)
  ())

(defmethod make-bindings ((namespace   equal-hash-table-bindings-mixin)
                          (environment t))
  (make-hash-table :test #'equal))

;;; `eq-hash-table-bindings-mixin'

(defclass eq-hash-table-bindings-mixin (hash-table-bindings-mixin)
  ())

(defmethod make-bindings ((namespace   eq-hash-table-bindings-mixin)
                          (environment t))
  (make-hash-table :test #'eq))
