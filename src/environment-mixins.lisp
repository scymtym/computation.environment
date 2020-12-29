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
                               &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (if-let ((bindings (namespace-bindings namespace environment)))
    (multiple-value-bind (value found?)
        (lookup-in-bindings name bindings namespace environment)
      (if found?
          (values value t environment)
          (values nil nil nil)))
    (values nil nil nil)))

(defmethod (setf lookup) ((new-value   t)
                          (name        t)
                          (namespace   t)
                          (environment bindings-mixin)
                          &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
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
               (or (lookup ,namespace 'namespace ,environment
                           :if-does-not-exist nil)
                   (error 'namespace-does-not-exist-error
                          :name        ,namespace
                          :environment ,environment)))))

  (defmethod entry-count-using-scope ((namespace   symbol)
                                      (environment meta-namespace-lookup-mixin)
                                      (scope       t))
    (let ((namespace (ensure-namespace environment namespace)))
      (entry-count-using-scope namespace environment scope)))

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
                                 &key if-does-not-exist)
    (let ((namespace (ensure-namespace environment namespace)))
      (lookup-using-scope name namespace environment scope
                          :if-does-not-exist if-does-not-exist)))

  (defmethod (setf lookup) ((new-value   t)
                            (name        t)
                            (namespace   symbol)
                            (environment meta-namespace-lookup-mixin)
                            &key if-does-not-exist)
    (let ((namespace (ensure-namespace environment namespace)))
      (setf (lookup name namespace environment
                    :if-does-not-exist if-does-not-exist)
            new-value))))

;;; `hierarchical-environment-mixin'

(defconstant +unbound+ '%unbound%)

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
                                    (scope       t))

  (if-let ((parent (parent environment)))
    (let ((result 0))
      (flet ((count-entry (name value environment)
               (declare (ignore name value environment))
               (incf result)))
        (declare (dynamic-extent #'count-entry))
        (map-entries-using-scope #'count-entry namespace environment scope)
        result))
    (call-next-method namespace environment scope)))

(defmethod map-entries-using-scope ((function    function)
                                    (namespace   t)
                                    (environment hierarchical-environment-mixin)
                                    (scope       (eql :direct)))
  (call-next-method (lambda (name value environment)
                      (unless (eq value '%unbound%)
                        (funcall function name value environment)))
                    namespace environment scope))

(defmethod map-entries-using-scope ((function    function)
                                    (namespace   t)
                                    (environment hierarchical-environment-mixin)
                                    (scope       (eql :all)))
  ;; Consider all entries that are not `+unbound+'. Ignore shadowing.
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
    ;; the same name in their ancestor environments. Do not call
    ;; FUNCTION for unbound entries, but still use the shadowing logic
    ;; in that case.
    (let ((seen (make-hash-table :test #'eq))) ; TODO depends on namespace
      (flet ((visit (name value environment)
               (unless (gethash name seen)
                 (setf (gethash name seen) t)
                 (unless (eq value +unbound+)
                   (funcall function name value environment)))))
        (declare (dynamic-extent #'visit))
        ;; `:direct*' is a private scope that currently relies on
        ;; implementation details to work. It is like `:direct' but
        ;; includes unbound entries.
        (map-entries-using-scope #'visit namespace environment :direct*)
        (map-entries-using-scope #'visit namespace parent      scope)))
    ;; If ENVIRONMENT does not have a PARENT, there are only direct
    ;; entries.
    (map-entries-using-scope function namespace environment :direct)))

(defmethod lookup-using-scope ((name        t)
                               (namespace   t)
                               (environment hierarchical-environment-mixin)
                               (scope       (eql t))
                               &key if-does-not-exist)
  (declare (ignore if-does-not-exist))
  (multiple-value-bind (value defined? container)
      (lookup-using-scope name namespace environment :direct
                          :if-does-not-exist nil)
    (cond ((eq defined? t)
           (values value defined? container))
          ((eq value +unbound+)
           (values nil nil nil))
          (t
           (when-let ((parent (parent environment)))
             (lookup-using-scope name namespace parent scope
                                 :if-does-not-exist nil))))))

;;; `bindings+hierarchical-environment-mixin'

(defclass bindings+hierarchical-environment-mixin
    (hierarchical-environment-mixin
     bindings-mixin)
  ())

#+later (defmethod make-bindings ((namespace   )
                          (environment )))

(defmethod entry-count-in-bindings :around ((bindings    hash-table)
                                            (namespace   hash-table-bindings-mixin)
                                            (environment bindings+hierarchical-environment-mixin))
  (- (call-next-method)
     (count +unbound+ (hash-table-values bindings) :test #'eq)))
