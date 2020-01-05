(cl:defpackage #:computation.environment.examples.defclass
  (:use
   #:cl
   #:alexandria)

  (:shadow
   #:defclass)

  (:local-nicknames
   (#:env #:computation.environment)))

(cl:in-package #:computation.environment.examples.defclass)

;;; Utilities

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interleave (list1 list2)
    (loop :for name  :in list1
          :for value :in list2
          :collect name :collect value)))

;;;

#+old (defclass names-are-symbols-mixin ()
        ())

#+old (defmethod env::make-bindings-table ((namespace names-are-symbols-mixin) environment)
  (let ((table (make-hash-table :test #'eq)))
    (values (lambda (name)
              (gethash name table))
            (lambda (new-value name)
              (setf (gethash name table) new-value))
            (lambda ()
              (hash-table-alist table)))))

(defvar *global-environment*
  (let ((environment (make-instance 'env::global-environment)))
    (setf (env:lookup 'class          'env::namespace environment) (make-instance 'env::eq-namespace)
          (env:lookup 'standard-class 'class          environment) (find-class 'standard-class))
    environment))

(defun ensure-superclass (name environment)
  (env:ensure name 'class environment
              (lambda ()
                (make-instance 'c2mop:forward-referenced-class :name name))))

(defun ensure-class (metaclass name superclass-names slots default-inits options
                     environment)
  (let* ((metaclass    (env:lookup metaclass 'class environment))
         (superclasses (map 'list (rcurry #'ensure-superclass environment)
                            superclass-names)))
    (macrolet ((init (function &rest arguments)
                 `(,function ,@arguments
                             :direct-superclasses     superclasses
                             :direct-slots            slots
                             :direct-default-initargs default-inits)))
      (env:make-or-update
       name 'class environment
       (lambda ()
         (init make-instance metaclass :name name))
       (lambda (existing) ; TODO reinitialize the name here?
         (if (eq (class-of existing) metaclass)
             (init reinitialize-instance existing)
             (init change-class existing metaclass)))))))

#+no (defmacro defclass (name superclass-names slots &rest options)
  ;; TODO need to parse slots and options for cases like
  ;; (let ((a 1))
  ;;   (cl:defclass whoop ()
  ;;     ((a :initform a))))
  `(ensure-class ',name '(,@superclass-names) '(,@slots) '(,@options) *global-environment*))

(defmacro defclass (&whole form &rest ignored)
  (declare (ignore ignored))
  (let ((info (syntax:parse t (syntax:find-syntax 'cl:defclass) `(cl:defclass ,@(rest form)))))
    (destructuring-bind (name superclass-names slots
                         default-initargs default-initforms
                         metaclass documentation ; option-names option-values
                         )
        (loop :for (name value raw) :on info :by #'cdddr
              :collect value)
      (flet ((slot (info)
               (destructuring-bind (name initargs readers writers accessors
                                    allocation initform type documentation
                                    option-names option-values)
                   (loop :for (name value raw) :on info :by #'cdddr
                         :collect value)
                 `(list :name          ',name
                        :initargs      '(,@initargs)
                        :readers       '(,@readers)
                        :writers       '(,@writers) ; '(,@accessors)
                        :initform      ',initform
                        :initfunction  (lambda () ,initform)
                        :allocation    ',allocation
                        :type          ',type
                        :documentation ,documentation
                        ,@(interleave option-names option-values))))
             (default-init (arg form)
               `(list ',arg ',form (lambda () ,form))))
        `(ensure-class ',(or metaclass 'standard-class)
                       ',name
                       '(,@superclass-names)
                       (list ,@(map 'list #'slot slots))
                       (list ,@(map 'list #'default-init
                                    default-initargs default-initforms))
                       '() ; ,(interleave option-names option-values)
                       *global-environment*)))))

;;; Test

(defclass foo-bar (standard-class) ())

(defmethod c2mop:validate-superclass ((class      standard-class #+should-be #.(env:lookup 'foo-bar 'class *global-environment*))
                                      (superclass standard-class))
  t)

(defclass bar ()
  ()
  (:metaclass foo-bar))

(defclass foo (bar fez) ())

(defclass fez () ())

(let ((e (make-instance 'env::lexical-environment :parent *global-environment*)))
  (setf (env:lookup 'bar 'class e) :foo)
  (list (env:lookup 'bar 'class e)
        (env:lookup 'standard-class 'class e)))

(let ((b 1))
  (defclass foo (bar)
    ((a :reader a
        :reader foo-a
        :initform b))
    (:default-initargs :a b)))
