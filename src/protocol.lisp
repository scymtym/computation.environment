;;;; protocol.lisp --- Protocol provided by the computation.environment system.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

;;; Bindings protocol

(defgeneric make-bindings (namespace environment))

(defgeneric entries-in-bindings (bindings namespace environment))

(defgeneric lookup-in-bindings (name bindings namespace environment))

(defgeneric (setf lookup-in-bindings) (new-value name bindings namespace environment))

;;; Environment protocol

;;; TODO accessors for namespaces (direct and indirect?)

(defgeneric entry-count (namespace environment))

;; TODO map-entries

(defgeneric entries (namespace environment)
  (:documentation
   "Return an alist of entries in NAMESPACE in ENVIRONMENT."))

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

(defgeneric (setf lookup) (new-value name namespace environment
                           &key
                           if-does-not-exist
                           if-exists)
  (:documentation
   "Set the value of (NAME NAMESPACE) in ENVIRONMENT to NEW-VALUE."))

(defgeneric make-or-update (name namespace environment make-cont update-cont))

(defgeneric ensure (name namespace environment make-cont))

;;; Default behavior

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
                                      'simple-error ; TODO
                                      :format-control "~A ~S not found in ~A"
                                      :format-arguments (list namespace name container))))
                      (funcall if-does-not-exist condition)))
          (t        if-does-not-exist)))))

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

; root

(defgeneric depth (environment))

(defgeneric direct-entries (namespace environment))

;;; TODO alternatively add &key scope to lookup where :scope t => lookup, :scope 1 => direct-lookup, :scope 2 => direct and parent, ...
(defgeneric direct-lookup (name namespace environment))

;;; Default behavior

(defmethod parent ((environment t))
  nil)

(defmethod depth ((environment t))
  (if-let ((parent (parent environment)))
    (1+ (depth parent))
    0))

;;; Entry update protocol

#+maybe (defgeneric ensure-using-existing (name namespace environment existing &rest args))
