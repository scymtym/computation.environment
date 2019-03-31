(cl:in-package #:environment)

;;; `bindings-mixin'

(defclass bindings-mixin ()
  ((%bindings :reader   %bindings
              :initform (make-hash-table :test #'eq))))

(defmethod print-items:print-items append ((object bindings-mixin))
  (let ((namespace-count (hash-table-count (%bindings object))))
    `((:namespace-count ,namespace-count "~D namespace~:P"))))

(declaim (inline namespace-bindings ensure-namespace-bindings))
(defun namespace-bindings (namespace environment)
  (when-let ((functions (gethash namespace (%bindings environment))))
    (values-list functions)))

(defun ensure-namespace-bindings (namespace environment)
  (values-list
   (ensure-gethash
    namespace (%bindings environment)
    (multiple-value-list (make-bindings-table namespace environment)))))

(defmethod make-bindings-table (namespace environment)
  (let ((table (make-hash-table :test #'equal)))
    (values (lambda (name)
              (gethash name table))
            (lambda (new-value name)
              (setf (gethash name table) new-value))
            (lambda ()
              (hash-table-alist table)))))

;;; Entries

(defmethod direct-entries ((namespace t) (environment bindings-mixin))
  (when-let ((entries (nth-value 2 (namespace-bindings namespace environment))))
    (funcall entries)))

(macrolet ((do-it (name namespace environment)
             `(if-let ((get (namespace-bindings ,namespace ,environment)))
                (funcall get ,name)
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
  (let ((set (nth-value 1 (ensure-namespace-bindings namespace environment))))
    (funcall set new-value name)))

;;; `meta-namespace-mixin'

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass namespace () ()))

(#+sbcl sb-ext:defglobal #-sbcl defvar **meta-namespace** (make-instance 'namespace))

(defclass meta-namespace-lookup-mixin ()
  ((%namespaces :initarg  :namespaces
                :accessor %namespaces)))

(macrolet ((ensure-namespace (environment namespace)
             (once-only (environment namespace)
               `(ensure-gethash
                 ,namespace (%namespaces ,environment)
                 (lookup ,namespace 'namespace ,environment)))))

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
            new-value))))

;;; `hierarchical-environment-mixin'

(defclass hierarchical-environment-mixin () ; TODO make a parented-mixin in cleaal.base?
  ((%parent :initarg  :parent
            :reader   parent
            :initform nil)))

(defmethod print-items:print-items append ((object hierarchical-environment-mixin))
  (let ((depth (depth object)))
    `((:depth ,depth " @~D" ((:after :namespace-count))))))

;;; Entries and lookup protocol

(defmethod entries ((namespace   t)
                    (environment hierarchical-environment-mixin))
  (if-let ((parent (parent environment)))
    (append (direct-entries namespace environment)
            (entries namespace parent))
    (direct-entries namespace environment)))

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
    (if defined?
        (values value defined?)
        (when-let ((parent (parent environment)))
          (lookup name namespace parent :if-does-not-exist nil)))))

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
