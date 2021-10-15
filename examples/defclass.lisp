;;;; defclass.lisp --- Example implementation of CL:DEFCLASS.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:computation.environment.examples.defclass
  (:use
   #:cl)

  (:shadow
   #:defclass)

  (:local-nicknames
   (#:a   #:alexandria)
   (#:env #:computation.environment)))

(cl:in-package #:computation.environment.examples.defclass)

;;; The global environment
;;;
;;; The global environment has a namespace `class' for classes. It
;;; also contains an entry for the name `standard-class' in that
;;; `class' namespace.

(cl:defclass class-namespace (env::eq-namespace)
  ())

(defvar *global-environment*
  (let ((environment (make-instance 'env::global-environment)))
    (setf (env:lookup 'class          'env::namespace environment) (make-instance 'class-namespace)
          (env:lookup 'standard-class 'class          environment) (find-class 'standard-class))
    environment))

;;; `ensure-class'
;;;
;;; `ensure-class' is the main part. Given a name and information
;;; describing the desired class, it checks the global environment and
;;; adds a new entry or updates an existing entry for the given name.

(defun ensure-superclass (name environment)
  ;; We use `ensure' so that if ENVIRONMENT already contains a class
  ;; for NAME, that class is used. Otherwise the thunk creates a
  ;; `forward-referenced-class' and NAME is bound to that instance in
  ;; ENVIRONMENT.
  (env:ensure name 'class environment
              (lambda ()
                (make-instance 'c2mop:forward-referenced-class :name name))))

(defun ensure-class (metaclass-name name superclass-names
                     slots default-inits other-initargs
                     environment)
  (let* ((metaclass    (env:lookup metaclass-name 'class environment))
         (superclasses (map 'list (a:rcurry #'ensure-superclass environment)
                            superclass-names)))
    (macrolet ((init (function &rest arguments)
                 `(apply #',function ,@arguments
                         :direct-superclasses     superclasses
                         :direct-slots            slots
                         :direct-default-initargs default-inits
                         other-initargs)))
      ;; The binding for NAME in ENVIRONMENT is in one of four states:
      ;; 1) non-existent 2) NAME is bound to a class with a different
      ;; name 3) NAME is bound to a class named NAME but the metaclass
      ;; is not METACLASS (in particular, the metaclass of the
      ;; existing class could be `forward-referenced-class') 4) NAME
      ;; is bound to a class named NAME and the metaclass is
      ;; METACLASS. We use `make-or-update' to create a new class if
      ;; necessary and otherwise update or replace the existing one.
      (env:make-or-update
       name 'class environment
       (lambda ()
         ;; This is called if NAME is not associated with a class in
         ;; ENVIRONMENT.
         (init make-instance metaclass :name name))
       (lambda (existing container)
         (declare (ignore container))
         ;; This is called if a class is already associated with NAME
         ;; in ENVIRONMENT.
         (cond ;; If the existing class has a different name, for
               ;; example as a result of
               ;;
               ;;   (defclass foo () ())
               ;;   (setf (find-class 'bar) (find-class 'foo))
               ;;   (defclass bar () ())
               ;;
               ;; , create a new instance.
               ((not (eq (class-name existing) name))
                (values (init make-instance metaclass :name name) t))
               ;; Depending on whether the existing class is a direct
               ;; instance of METACLASS, we try to change the class of
               ;; the existing class or just reinitialize it.
               ((not (eq (class-of existing) metaclass))
                (init change-class existing metaclass))
               (t
                (init reinitialize-instance existing))))))))

;;; Syntax part

;;; Note: this is not a proper parser for DEFCLASS forms. It is
;;; minimal to keep the example focused on environments.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-slot (expression)
    (destructuring-bind (name &rest options) expression
      (let ((initargs       '())
            (readers        '())
            (writers        '())
            (initform       nil)
            (allocation     nil)
            (type           nil)
            (documentation  nil)
            (other-initargs '()))
        (loop :for (name value) :on options :by #'cddr
              :do (case name
                    (:initarg       (push value initargs))
                    (:reader        (push value readers))
                    (:writer        (push value writers))
                    (:accessor      (push value readers)
                                    (push `(setf ,value) writers))
                    (:initform      (setf initform value))
                    (:allocation    (setf allocation value))
                    (:type          (setf type value))
                    (:documentation (setf documentation value))
                    (t              (a:appendf other-initargs
                                               (list name value)))))
        (list name initargs readers writers initform allocation type
              documentation other-initargs))))

  (defun parse-defclass (rest)
    (destructuring-bind (name superclass-names slots &rest options) rest
      (let ((slots            (map 'list #'parse-slot slots))
            (metaclass-name   (second (assoc :metaclass options)))
            (default-initargs (rest (assoc :default-initargs options)))
            (documentation    (second (assoc :documentation options)))
            (other-initargs   (loop :for (name . value) :in options
                                    :unless (member name '(:metaclass
                                                           :default-initargs
                                                           :documentation))
                                    :collect name :and :collect value)))
        (values name superclass-names slots metaclass-name default-initargs
                documentation other-initargs)))))

(defmacro defclass (#+no &whole #+no form &rest rest)
  (multiple-value-bind (name superclass-names slots metaclass-name
                        default-initargs documentation other-initargs)
      (parse-defclass rest)
    (flet ((slot (info)
             (destructuring-bind (name initargs readers writers
                                  initform allocation type documentation
                                  options)
                 info
               `(list :name          ',name
                      :initargs      '(,@initargs)
                      :readers       '(,@readers)
                      :writers       '(,@writers)
                      :initform      ',initform
                      :initfunction  (lambda () ,initform)
                      :allocation    ',(or allocation :instance)
                      :type          ',type
                      :documentation ,documentation
                      ,@(loop :for (name value) :on options :by #'cddr
                              :collect name :collect `',value))))
           (default-init (arg form)
             `(list ',arg ',form (lambda () ,form))))
      `(ensure-class ',(or metaclass-name 'standard-class)
                     ',name
                     '(,@superclass-names)
                     (list ,@(map 'list #'slot slots))
                     (list ,@(loop :for (name value) :on default-initargs :by #'cddr
                                   :collect (default-init name value)))
                     (list ,@(loop :for (name value) :on other-initargs :by #'cddr
                                   :collect `',name :collect `',value)
                           ,@(when documentation
                               `(:documentation ,documentation)))
                     *global-environment*))))

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

(let ((b 1)
      (c 2))
  (defclass foo (bar)
    ((%a :initarg :a
         :reader   a
         :reader   foo-a
         :accessor access
         :initform c))
    (:default-initargs :a b)
    (:documentation "bla")))

(defclass new (forward)
  ())

;;; Only reinitialize if the name of the class matches its environment
;;; entry.

(defclass real-one () ())

(setf (env:lookup 'fake-one 'class *global-environment*)
      (env:lookup 'real-one 'class *global-environment*))

(assert (eq (env:lookup 'fake-one 'class *global-environment*)
            (env:lookup 'real-one 'class *global-environment*)))

(defclass fake-one () ())

(assert (not (eq (env:lookup 'fake-one 'class *global-environment*)
                 (env:lookup 'real-one 'class *global-environment*))))
