(cl:defpackage #:computation.environment.examples.interpreter
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:env #:computation.environment)))

(cl:in-package #:computation.environment.examples.interpreter)

;;; Global environment setup

(defvar *variable-namespace* (make-instance 'env::eq-namespace))

(defvar *function-namespace* (make-instance 'env::eq-namespace))

(defvar *tag-namespace* (make-instance 'env::eq-namespace))

(defclass global-environment (env:global-environment)
  ())

(defmethod initialize-instance :after ((instance global-environment) &key)
  (setf (env:lookup 'variable 'env:namespace instance) *variable-namespace*
        (env:lookup 'function 'env:namespace instance) *function-namespace*
        (env:lookup 'tag      'env:namespace instance) *tag-namespace*))

(defvar *global-environment*
  (let ((env (make-instance 'global-environment)))
    (setf (env:lookup '+       'function env) #'+
          (env:lookup 'list    'function env) #'list
          (env:lookup 'funcall 'function env) #'funcall)
    env))

;;; Utilities

(defun make-environment (parent)
  (make-instance 'env:lexical-environment :parent parent))

(defun evaluate-body-forms (forms environment)
  (map nil (rcurry #'evaluate environment) (butlast forms))
  (evaluate (lastcar forms) environment))

(defun make-abstraction (lambda-list body environment)
  (lambda (&rest args)
    (let ((environment (make-environment environment)))
      (loop :for name  :in lambda-list
            :for value :in args
            :do (setf (env:lookup name 'variable environment)
                      value))
      (evaluate-body-forms body environment))))

;;;

(defmethod evaluate ((form symbol) (environment env:lexical-environment))
  (env:lookup form 'variable environment))

(defmethod evaluate ((form t) (environment env:lexical-environment))
  form)

(defmethod evaluate ((form cons) (environment env:lexical-environment))
  (evaluate-using-head (first form) form environment))

(defmethod evaluate-using-head ((head        (eql 'progn))
                                (form        t)
                                (environment env:lexical-environment))
  (evaluate-body-forms (rest form) environment))

(defmethod evaluate-using-head ((head        (eql 'let))
                                (form        t)
                                (environment env:lexical-environment))
  (let ((environment (make-environment environment)))
    (destructuring-bind (bindings &rest forms) (rest form)
      (loop :for (name value-form) :in bindings
            :do (setf (env:lookup name 'variable environment)
                      (evaluate value-form environment)))
      (evaluate-body-forms forms environment))))

(defmethod evaluate-using-head ((head        (eql 'setq))
                                (form        t)
                                (environment env:lexical-environment))
  (destructuring-bind (name new-value-form) (rest form)
    (let ((new-value (evaluate new-value-form environment)))
      ;; TODO maybe use `make-or-update'?
      (multiple-value-bind (value found? container)
          (env:lookup name 'variable environment)
        (declare (ignore value found?))
        (setf (env:lookup name 'variable container) new-value)))))

(defmethod evaluate-using-head ((head        (eql 'labels))
                                (form        t)
                                (environment env:lexical-environment))
  (let ((environment (make-environment environment)))
    (destructuring-bind (bindings &rest forms) (rest form)
      (map nil (lambda (binding)
                 (destructuring-bind (name lambda-list &rest body) binding
                   (setf (env:lookup name 'function environment)
                         (make-abstraction lambda-list body environment))))
           bindings)
      (evaluate-body-forms forms environment))))

(defmethod evaluate-using-head ((head        (eql 'function))
                                (form        t)
                                (environment env:lexical-environment))
  (env:lookup (second form) 'function environment))

(defmethod evaluate-using-head ((head        symbol)
                                (form        t)
                                (environment env:lexical-environment))
  (let ((function  (env:lookup head 'function environment))
        (arguments (map 'list (rcurry #'evaluate environment)
                        (rest form))))
    (apply function arguments)))

(defmethod evaluate-using-head ((head        (eql 'quote))
                                (form        t)
                                (environment env:lexical-environment))
  (second form))

(defmethod evaluate-using-head ((head        (eql 'catch))
                                (form        t)
                                (environment env:lexical-environment))
  (destructuring-bind (tag &body body) (rest form)
    (let* ((tag          (evaluate tag environment))
           (continuation (lambda (value)
                           (return-from evaluate-using-head value)))
           (environment  (env:augmented-environment
                          environment `((,tag . tag)) (list continuation))))
      (evaluate-body-forms body environment))))

(defmethod evaluate-using-head ((head        (eql 'throw))
                                (form        t)
                                (environment env:lexical-environment))
  (destructuring-bind (tag value) (rest form)
    (let ((tag   (evaluate tag environment))
          (value (evaluate value environment)))
      (clouseau:inspect environment :new-process t)
      (funcall (env:lookup tag 'tag environment) value))))

(defmethod evaluate-using-head ((head        (eql 'lambda))
                                (form        t)
                                (environment env:lexical-environment))
  (destructuring-bind (lambda-list &body body) (rest form)
    (make-abstraction lambda-list body environment)))

;;; Test

(let ((env (make-instance 'env:lexical-environment :parent *global-environment*)))
  (evaluate `(let ((d 1000))
               (labels ((foo (a b c)
                          (setq d (+ d d))
                          (list a (+ b c d)))
                        (bar ()
                          d))
                 (list (foo (+ (let ((a 1)) (progn (+ a 2)))
                               (let ((a 5)) a))
                            10 11)
                       (bar)
                       (funcall (catch 'foo (lambda () (+ 42 (throw 'foo 42))))))))
            env))
