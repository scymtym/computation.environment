;;;; protocol.lisp --- Protocol provided by the computation.environment system.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

;;; Bindings protocol
;;;
;;; This protocol allows accessing the bindings within a single
;;; namespace in one particular environment.

(defgeneric make-bindings (namespace environment)
  (:documentation
   "Return a bindings object for NAMESPACE in ENVIRONMENT.

    The returned object must be usable with NAMESPACE and ENVIRONMENT
    in the bindings protocol."))

(defgeneric entry-count-in-bindings (bindings namespace environment)
  (:documentation
   "Return the number of entries in BINDINGS in NAMESPACE, ENVIRONMENT."))

(defgeneric map-entries-in-bindings (function bindings namespace environment))

(defgeneric lookup-in-bindings (name bindings namespace environment))

(defgeneric (setf lookup-in-bindings) (new-value name bindings namespace environment))

;;; Environment protocol
;;;
;;; This protocol allows accessing the bindings in all namespaces in a
;;; given scope starting at a particular environment. The scope
;;; controls, for example, whether bindings inherited from parent
;;; environments should be considered.

;;; TODO accessors for namespaces (direct and indirect?)

(defmacro define-scoped-protocol-function (name lambda-list &rest options)
  (multiple-value-bind (required optional rest keys)
      (parse-ordinary-lambda-list lambda-list)
    (when optional
      (error "Cannot use ~S parameters" '&optional))
    (when rest
      (error "Cannot use ~S parameter" '&rest))
    (let* ((using-scope-name        (symbolicate name '#:-using-scope))
           (key-parameters/defaults (map 'list #'butlast keys))
           (key-parameters          (map 'list #'butlast
                                         key-parameters/defaults)))
      `(progn
         (defgeneric ,name (,@required &key ,@key-parameters scope)
           (:method (,@(map 'list (rcurry #'list t) required)
                     &key ,@key-parameters/defaults (scope t))
             (,using-scope-name
              ,@required scope
              ,@(mappend (lambda (parameter)
                           (destructuring-bind ((keyword var)) parameter
                             (list keyword var)))
                         key-parameters)))
           ,@options)
         (defgeneric ,using-scope-name (,@required scope
                                        ,@(when key-parameters
                                            `(&key ,@key-parameters)))
           ,@options)))))

(define-scoped-protocol-function entry-count (namespace environment)
  (:documentation
   "Return the number of entries in NAMESPACE in ENVIRONMENT for SCOPE."))

(define-scoped-protocol-function map-entries (function namespace environment)
  (:documentation
   "Call FUNCTION for each entry in NAMESPACE in ENVIRONMENT for SCOPE.

    The lambda list of FUNCTION must be compatible with

      (name value container)"))

(define-scoped-protocol-function entries (namespace environment)
  (:documentation
   "Return entries in NAMESPACE in ENVIRONMENT for SCOPE as an alist."))

(define-scoped-protocol-function lookup (name namespace environment
                                         &key (if-does-not-exist #'error))
  (:documentation
   "Lookup and return the value for NAME in NAMESPACE in ENVIRONMENT for SCOPE.

    Return three values: 1) the found value (subject to
    IF-DOES-NOT-EXIST) 2) a Boolean indicating whether a value exists
    3) the environment in which the value was found.

    SCOPE controls which bindings are considered. Examples of scopes
    include binding directly contained in ENVIRONMENT and bindings
    contained in ENVIRONMENT or any of its ancestor environments.

    IF-DOES-NOT-EXIST controls the behavior in case such a value does
    not exist."))

(defgeneric (setf lookup) (new-value name namespace environment ; TODO separate mutable environment protocol?
                           &key if-does-not-exist)
  (:documentation
   "Set the value of NAME in NAMESPACE in ENVIRONMENT to NEW-VALUE."))

(defgeneric make-or-update (name namespace environment make-cont update-cont
                            &key scope)
  (:documentation
   "Use MAKE-CONT or UPDATE-CONT to set NAME in NAMESPACE in ENVIRONMENT for SCOPE.

    Return four values: 1) the new value of NAME in NAMESPACE in
    ENVIRONMENT 2) a Boolean indicating whether the value of NAME in
    NAMESPACE in ENVIRONMENT has been updated 3) the previous value of
    NAME in NAMESPACE in ENVIRONMENT 4) the container in which the
    previous value was found.

    If no value exists for NAME in NAMESPACE in ENVIRONMENT, MAKE-CONT
    is called to make a value which is then set as the value of NAME
    in NAMESPACE in ENVIRONMENT.

    If a value exists for NAME in NAMESPACE in environment,
    UPDATE-CONT is called with the existing value and its container to
    potentially compute an updated value. If an updated value is
    computed, that value is set as the value of NAME in NAMESPACE in
    ENVIRONMENT.

    MAKE-CONT has to be a function with a lambda list compatible to

      ()

    and has to return the new value as its primary return value.

    UPDATE-CONT has to be a function with a lambda list compatible to

      (old-value old-container)

    and must return between two values and three values when called:
    1) an updated value based on OLD-VALUE 2) a Boolean indicating
    whether the first return value is different from OLD-VALUE 3)
    optionally a container in which the returned updated value should
    be set."))

(defgeneric ensure (name namespace environment make-cont &key scope)
  (:documentation
   "Maybe use MAKE-CONT to set NAME in NAMESPACE in ENVIRONMENT for SCOPE.

    Return four values: 1) the new value of NAME in NAMESPACE in
    ENVIRONMENT 2) a Boolean indicating whether the value of NAME in
    NAMESPACE in ENVIRONMENT has been updated 3) the container in
    which the previous value was found.

    If no value exists for NAME in NAMESPACE in ENVIRONMENT, MAKE-CONT
    is called to make a value which is then set as the value of NAME
    in NAMESPACE in ENVIRONMENT.

    MAKE-CONT has to be a function with a lambda list compatible to

      ()

    and has to return the new value as its primary return value."))

;;; Default behavior

(defmethod entries-using-scope ((namespace t) (environment t) (scope t))
  (let ((result '()))
    (flet ((collect (name value environment)
             (declare (ignore environment))
             (push (cons name value) result)))
      (declare (dynamic-extent #'collect))
      (map-entries-using-scope #'collect namespace environment scope))
    result))

(defmethod lookup-using-scope :around ((name      t)
                                       (namespace t) ; standard-object
                                       (container t)
                                       (scope     t)
                                       &key
                                       (if-does-not-exist #'error))
  (multiple-value-bind (value value? direct-container) (call-next-method)
    (if value?
        (values value value? direct-container)
        (typecase if-does-not-exist
          (function (let ((condition (make-condition
                                      'entry-does-not-exist-error
                                      :environment container
                                      :name        name
                                      :namespace   namespace)))
                      (funcall if-does-not-exist condition)))
          (t        (values if-does-not-exist nil nil))))))

(defmethod make-or-update ((name        t)
                           (namespace   t)
                           (environment t)
                           (make-cont   t)
                           (update-cont t)
                           &key (scope t))
  ;; 1. Look up an existing value.
  (multiple-value-bind (value value? container)
      (lookup name namespace environment :scope             scope
                                         :if-does-not-exist nil)
    ;; 2. Update the existing value (may or may not yield a new value)
    ;;    or make a value (yields a new value unconditionally).
    (multiple-value-bind (new-value new-value? new-container)
        (if value?
            (funcall update-cont value container)
            (values (funcall make-cont) t))
      ;; 3. If there is a new value, install it, otherwise return the
      ;;    unchanged existing value.
      (if new-value?
          (let ((new-container (or new-container environment)))
            (setf (lookup name namespace new-container) new-value)
            (values new-value t value container))
          (values value nil value container)))))

(defmethod ensure ((name t) (namespace t) (environment t) (make-cont t)
                   &key (scope t))
  (flet ((keep-it (value container)
           (declare (ignore container))
           (values value nil)))
    (declare (dynamic-extent #'keep-it))
    (multiple-value-bind (new-value updated? old-value old-container)
        (make-or-update name namespace environment make-cont #'keep-it
                        :scope scope)
      (declare (ignore old-value))
      (values new-value updated? old-container))))

;;; Hierarchical environment protocol

(defgeneric parent (environment))

(defgeneric root (environment)
  (:documentation
   "Return the ancestor of ENVIRONMENT that has no parent."))

(defgeneric depth (environment))

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
