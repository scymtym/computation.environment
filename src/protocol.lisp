;;;; protocol.lisp --- Protocol provided by the computation.environment system.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

;;; Bindings protocol
;;;
;;; This protocol allows accessing the bindings within a single
;;; namespace in one particular environment.

(defgeneric make-bindings (namespace environment))

(defgeneric entry-count-in-bindings (bindings namespace environment))

(defgeneric map-entries-in-bindings (function bindings namespace environment))

(defgeneric entries-in-bindings (bindings namespace environment)) ; TODO remove

(defgeneric lookup-in-bindings (name bindings namespace environment))

(defgeneric (setf lookup-in-bindings) (new-value name bindings namespace environment))

;;; Environment protocol
;;;
;;; This protocol allows accessing the bindings in all namespaces in a
;;; given scope starting at a particular environment. The scope
;;; controls, for example, whether bindings inherited from parent
;;; environments should be considered.

;;; TODO accessors for namespaces (direct and indirect?)

(defgeneric entry-count (namespace environment))

(defgeneric map-entries (function namespace environment))

(defgeneric entries (namespace environment)
  (:documentation
   "Return an alist of entries in NAMESPACE in ENVIRONMENT."))

(defgeneric map-effective-entries (function namespace environment)
  (:documentation
   "Call FUNCTION for each entry in NAMESPACE in ENVIRONMENT.

    The lambda list of FUNCTION must be compatible with

      (name value container)"))

(defgeneric effective-entries (namespace environment)
  (:documentation
   "TODO"))

(defgeneric lookup (name namespace environment
                    &key
                    if-does-not-exist
                    if-exists)
  (:documentation
   "Lookup and return the value for NAME in NAMESPACE in ENVIRONMENT.

    Return two values: 1) the found value (subject to
    IF-DOES-NOT-EXIST) 2) a Boolean indicating whether a value exists.

    IF-DOES-NOT-EXIST controls the behavior in case such a value does
    not exist.

    IF-EXISTS is accepted and ignored for parity with `(setf
    lookup)'."))

(defgeneric (setf lookup) (new-value name namespace environment ; TODO separate mutable environment protocol?
                           &key
                           if-does-not-exist
                           if-exists)
  (:documentation
   "Set the value of (NAME NAMESPACE) in ENVIRONMENT to NEW-VALUE."))

(defgeneric make-or-update (name namespace environment make-cont update-cont)
  (:documentation
   "TODO

    UPDATE-CONT has to be a function with a lambda-list compatible to

      (old-value)

    and has to return two values when called: 1) an updated value
    based on OLD-VALUE 2) a Boolean indicating whether the first
    return value is different from OLD-VALUE."))

(defgeneric ensure (name namespace environment make-cont)
  (:documentation
   "TODO"))

;;; Default behavior

(defmethod entries ((namespace t) (environment t))
  (let ((result '()))
    (flet ((collect (name value scope)
             (declare (ignore scope)) ; TODO include scope in result?
             (push (cons name value) result)))
      (declare (dynamic-extent #'collect))
      (map-entries #'collect namespace environment))
    result))

(defmethod map-entries ((function function) (namespace t) (environment t))
  (map-effective-entries (rcurry function environment) namespace environment))

(defmethod effective-entries ((namespace t) (environment t))
  (let ((result '()))
    (flet ((collect (name value)
             (push (cons name value) result)))
      (declare (dynamic-extent #'collect))
      (map-effective-entries #'collect namespace environment))
    result))

(defmethod lookup :around ((name      t)
                           (namespace t) ; standard-object
                           (container t)
                           &key
                           (if-does-not-exist #'error)
                           if-exists)
  (declare (ignore if-exists))
  (multiple-value-bind (value value?) (call-next-method)
    (if value?
        (values value value?)
        (typecase if-does-not-exist
          (function (let ((condition (make-condition
                                      'entry-does-not-exist-error
                                      :environment container
                                      :name        name
                                      :namespace   namespace)))
                      (funcall if-does-not-exist condition)))
          (t        (values if-does-not-exist nil))))))

;;; TODO could return three values: old, updated, new
(defmethod make-or-update ((name        t)
                           (namespace   t)
                           (environment t)
                           (make-cont   t)
                           (update-cont t))
  (multiple-value-bind (value value?) (lookup name namespace environment
                                              :if-does-not-exist nil)
    (multiple-value-bind (new-value new-value?)
        (if value?
            (funcall update-cont value)
            (values (funcall make-cont) t))
      (if new-value?
          (values (setf (lookup name namespace environment) new-value) t)
          (values value                                                nil)))))

(defmethod ensure ((name t) (namespace t) (environment t) (make-cont t))
  (make-or-update name namespace environment make-cont #'identity))

;;; Hierarchical environment protocol

(defgeneric parent (environment))

(defgeneric root (environment)
  (:documentation
   "Return the ancestor of ENVIRONMENT that has no parent."))

(defgeneric depth (environment))

(defgeneric direct-entry-count (namespace environment))

(defgeneric map-direct-entries (function namespace environment))

(defgeneric direct-entries (namespace environment))

;;; TODO alternatively add &key scope to lookup where :scope t => lookup, :scope 1 => direct-lookup, :scope 2 => direct and parent, ...
;;; TODO second alternative: add &key filter which gets called with each environment before its entries are considered
(defgeneric direct-lookup (name namespace environment))

;;; Default behavior

(defmethod parent ((environment t))
  nil)

(defmethod root ((environment t))
  (if-let ((parent (parent environment)))
    (root parent)
    environment))

(defmethod depth ((environment t))
  (if-let ((parent (parent environment)))
    (1+ (depth parent))
    0))

(defmethod direct-entries ((namespace t) (environment t))
  (let ((result '()))
    (flet ((collect (name value)
             (push (cons name value) result)))
      (declare (dynamic-extent #'collect))
      (map-direct-entries #'collect namespace environment))
    result))

;;; Entry update protocol

#+maybe (defgeneric ensure-using-existing (name namespace environment existing &rest args))
