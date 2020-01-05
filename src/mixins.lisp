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

(macrolet ((do-it (namespace environment)
             `(if-let ((bindings (namespace-bindings ,namespace ,environment)))
                (entry-count-in-bindings bindings ,namespace ,environment)
                0)))

  (defmethod direct-entry-count ((namespace t) (environment bindings-mixin))
    (do-it namespace environment))

  (defmethod entry-count ((namespace t) (environment bindings-mixin))
    (do-it namespace environment)))

(macrolet ((do-it (function namespace environment)
             `(when-let ((bindings (namespace-bindings ,namespace ,environment)))
                (map-entries-in-bindings ,function bindings ,namespace ,environment))))

  (defmethod map-direct-entries ((function    function)
                                 (namespace   t)
                                 (environment bindings-mixin))
    (do-it function namespace environment))

  (defmethod map-effective-entries ((function    function)
                                    (namespace   t)
                                    (environment bindings-mixin))
    (do-it function namespace environment)))

(macrolet ((do-it (name namespace environment)
             `(if-let ((bindings (namespace-bindings ,namespace ,environment)))
                (lookup-in-bindings ,name bindings ,namespace ,environment)
                (values nil nil))))

  (defmethod direct-lookup ((name t) (namespace t) (environment bindings-mixin))
    (do-it name namespace environment))

  (defmethod lookup ((name t) (namespace t) (environment bindings-mixin)
                     &key if-does-not-exist if-exists)
    (declare (ignore if-does-not-exist if-exists))
    (do-it name namespace environment)))

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

  (defmethod entry-count ((namespace   symbol)
                          (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (entry-count namespace environment)))

  (defmethod map-entries ((function    function)
                          (namespace   symbol)
                          (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (map-entries function namespace environment)))

  (defmethod map-effective-entries ((function    function)
                                    (namespace   symbol)
                                    (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (map-effective-entries function namespace environment)))

  (defmethod entries ((namespace   symbol)
                      (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (entries namespace environment)))

  (defmethod lookup ((name        t)
                     (namespace   symbol)
                     (environment meta-namespace-lookup-mixin)
                     &key
                     if-does-not-exist
                     if-exists)
    (let ((namespace (ensure-namespace environment namespace)))
      (lookup name namespace environment
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
            new-value)))

  (defmethod direct-entry-count ((namespace   symbol)
                                 (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (direct-entry-count namespace environment)))

  (defmethod direct-entries ((namespace   symbol)
                             (environment meta-namespace-lookup-mixin))
    (let ((namespace (ensure-namespace environment namespace)))
      (direct-entries namespace environment))))

;;; `hierarchical-environment-mixin'

(defclass hierarchical-environment-mixin ()
  ((%parent :initarg  :parent
            :reader   parent
            :initform nil)))

(defmethod print-items:print-items append ((object hierarchical-environment-mixin))
  (let ((depth (depth object)))
    `((:depth ,depth " @~D" ((:after :namespace-count))))))

;;; Entries and lookup protocol

(defmethod entry-count ((namespace   t)
                        (environment hierarchical-environment-mixin))
  (+ (direct-entry-count namespace environment)
     (if-let ((parent (parent environment)))
       (entry-count namespace parent)
       0)))

(defmethod map-entries ((function    function)
                        (namespace   t)
                        (environment hierarchical-environment-mixin))
  (map-direct-entries (rcurry function environment) namespace environment)
  (when-let ((parent (parent environment)))
    (map-entries function namespace parent)))

(defmethod map-effective-entries ((function    function)
                                  (namespace   t)
                                  (environment hierarchical-environment-mixin))
  (if-let ((parent (parent environment)))
    (let ((seen (make-hash-table :test #'eq))) ; TODO depends on namespace
      (flet ((visit (name value)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (funcall function name value))))
        (declare (dynamic-extent #'visit))
        (map-direct-entries #'visit namespace environment)
        (map-effective-entries #'visit namespace parent)))
    (map-direct-entries function namespace environment)))

(defmethod lookup ((name        t)
                   (namespace   t)
                   (environment hierarchical-environment-mixin)
                   &key
                   if-does-not-exist
                   if-exists)
  (declare (ignore if-does-not-exist if-exists))
  (multiple-value-bind (value defined?)
      (direct-lookup name namespace environment ; :if-does-not-exist if-does-not-exist
                     )
    (cond ((eq defined? t)
           (values value defined?))
          ((eq value +unbound+)
           (values nil nil))
          (t
           (when-let ((parent (parent environment)))
             (lookup name namespace parent :if-does-not-exist nil))))))

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
