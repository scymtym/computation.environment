(cl:in-package #:computation.environment)

;;; `bindings-mixin'
;;;
;;; An environment mixin that adds binding storage to environment
;;; instances.

(defclass bindings-mixin ()
  ((%bindings :reader   %bindings
              :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object bindings-mixin))
  (let ((namespace-count (hash-table-count (%bindings object))))
    `((:namespace-count ,namespace-count "~D namespace~:P"))))

(declaim (inline namespace-bindings ensure-namespace-bindings))
(defun namespace-bindings (namespace environment)
  (gethash namespace (%bindings environment)))

(defun ensure-namespace-bindings (namespace environment)
  (ensure-gethash namespace (%bindings environment)
                  (make-bindings namespace environment)))

;;; Entries

(defmethod entry-count-using-scope ((namespace   t)
                                    (environment bindings-mixin)
                                    (scope       t))
  (if-let ((bindings (namespace-bindings namespace environment)))
    (entry-count-in-bindings bindings namespace environment)
    0))

(defmethod map-entries-using-scope ((function    function)
                                    (namespace   t)
                                    (environment bindings-mixin)
                                    (scope       t))
  (when-let ((bindings (namespace-bindings namespace environment)))
    (flet ((visit (name value)
             (funcall function name value environment)))
      (declare (dynamic-extent #'visit))
      (map-entries-in-bindings #'visit bindings namespace environment))))

(defmethod lookup-using-scope ((name        t)
                               (namespace   t)
                               (environment bindings-mixin)
                               (scope       t)
                               &key if-does-not-exist
                                    if-exists)
  (declare (ignore if-does-not-exist if-exists))
  (if-let ((bindings (namespace-bindings namespace environment)))
    (lookup-in-bindings name bindings namespace environment)
    (values nil nil)))

(defmethod (setf lookup) ((new-value   t)
                          (name        t)
                          (namespace   t)
                          (environment bindings-mixin)
                          &key if-does-not-exist if-exists)
  (declare (ignore if-does-not-exist if-exists))
  (let ((bindings (ensure-namespace-bindings namespace environment)))
    (setf (lookup-in-bindings name bindings namespace environment) new-value)))

;;; `meta-namespace-lookup-mixin'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass eq-namespace (eq-hash-table-bindings-mixin)
    ()))

(#+sbcl sb-ext:defglobal #-sbcl defvar **meta-namespace**
  (make-instance 'eq-namespace))

(defclass equal-namespace (equal-hash-table-bindings-mixin)
  ())

(defclass meta-namespace-lookup-mixin () ; TODO either remove the slot or initialize it
  ((%namespaces :initarg  :namespaces
                :accessor %namespaces)))

(macrolet ((ensure-namespace (environment namespace)
             `(ensure-gethash
               ,namespace (%namespaces ,environment)
               (lookup ,namespace 'namespace ,environment))))

  (defmethod entry-count-using-scope ((namespace   symbol)
                                      (environment meta-namespace-lookup-mixin)
                                      (scope       t))
    (let ((namespace (ensure-namespace environment namespace)))
      (entry-count-using-scope namespace environment t)))

  (defmethod map-entries-using-scope ((function    function)
                                      (namespace   symbol)
                                      (environment meta-namespace-lookup-mixin)
                                      (scope       t))
    (let ((namespace (ensure-namespace environment namespace)))
      (map-entries-using-scope function namespace environment scope)))

  (defmethod lookup-using-scope ((name        t)
                                 (namespace   symbol)
                                 (environment meta-namespace-lookup-mixin)
                                 (scope       t)
                                 &key
                                 if-does-not-exist
                                 if-exists)
    (let ((namespace (ensure-namespace environment namespace)))
      (lookup-using-scope name namespace environment scope
                          :if-does-not-exist if-does-not-exist
                          :if-exists         if-exists)))

  (defmethod (setf lookup) ((new-value   t)
                            (name        t)
                            (namespace   symbol)
                            (environment meta-namespace-lookup-mixin)
                            &key
                            if-does-not-exist
                            if-exists)
    (let ((namespace (ensure-namespace environment namespace)))
      (setf (lookup name namespace environment
                    :if-does-not-exist if-does-not-exist
                    :if-exists         if-exists)
            new-value))))

;;; `hierarchical-environment-mixin'

(defclass hierarchical-environment-mixin ()
  ((%parent :initarg  :parent
            :reader   parent
            :initform nil)))

(defmethod print-items:print-items append ((object hierarchical-environment-mixin))
  (let ((depth (depth object)))
    `((:depth ,depth " @~D" ((:after :namespace-count))))))

;;; Entries and lookup protocol

(defmethod entry-count-using-scope ((namespace   t)
                                    (environment hierarchical-environment-mixin)
                                    (scope       (eql t)))
  (+ (entry-count-using-scope namespace environment :direct)
     (if-let ((parent (parent environment)))
       (entry-count-using-scope namespace parent scope)
       0)))

(defmethod map-entries-using-scope ((function    function)
                                    (namespace   t)
                                    (environment hierarchical-environment-mixin)
                                    (scope       (eql :all)))
  (map-entries-using-scope function namespace environment :direct)
  (when-let ((parent (parent environment)))
    (map-entries-using-scope function namespace parent scope)))

(defmethod map-entries-using-scope ((function    function)
                                    (namespace   t)
                                    (environment hierarchical-environment-mixin)
                                    (scope       (eql t)))
  (if-let ((parent (parent environment)))
    ;; If ENVIRONMENT has a PARENT, consider the entries in PARENT
    ;; (and all ancestors) but only call FUNCTION when encountering a
    ;; name for the first time, that is environments shadow entries of
    ;; the same name in their ancestor environments.
    (let ((seen (make-hash-table :test #'eq))) ; TODO depends on namespace
      (flet ((visit (name value environment)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (funcall function name value environment))))
        (declare (dynamic-extent #'visit))
        (map-entries-using-scope #'visit namespace environment :direct)
        (map-entries-using-scope #'visit namespace parent      scope)))
    ;; If ENVIRONMENT does not have a PARENT, there are only direct
    ;; entries.
    (map-entries-using-scope function namespace environment :direct)))

(defmethod lookup-using-scope ((name        t)
                               (namespace   t)
                               (environment hierarchical-environment-mixin)
                               (scope       (eql t))
                               &key
                               if-does-not-exist
                               if-exists)
  (declare (ignore if-does-not-exist if-exists))
  (multiple-value-bind (value defined?)
      (lookup-using-scope name namespace environment :direct
                          :if-does-not-exist nil)
    (cond ((eq defined? t)
           (values value defined?))
          ((eq value +unbound+)
           (values nil nil))
          (t
           (when-let ((parent (parent environment)))
             (lookup-using-scope name namespace parent scope
                                 :if-does-not-exist nil))))))

;;; `bindings+hierarchical-environment-mixin'

(defclass bindings+hierarchical-environment-mixin
    (bindings-mixin
     hierarchical-environment-mixin)
  ())

#+later (defmethod make-bindings ((namespace   )
                          (environment )))

;;; `ensure-using-existing-mixin'

#+maybe (defclass ensure-using-existing-mixin ()
  ())

#+maybe (defmethod ensure ((name        t)
                   (namespace   t)
                   (environment ensure-using-existing-mixin)
                   &key
                   (if-does-not-exist (lambda ()
                                        (ensure-using-existing
                                         name namespace environment nil)))
                   (if-exists         (lambda (value)
                                        (ensure-using-existing
                                         name namespace environment value))))
  (call-next-method name namespace environment
                    :if-does-not-exist if-does-not-exist
                    :if-exists         if-exists))
