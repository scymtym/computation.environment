(cl:defpackage #:computation.environment.examples.interpreter
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:env #:computation.environment))

  (:shadow
   #:eval))

(cl:in-package #:computation.environment.examples.interpreter)

;;; Global environment setup

(defclass function-namespace (env::equal-namespace)
  ())

(defvar *function-namespace* (make-instance 'function-namespace))

;; TODO put this protocol into the actual library
(defmethod valid-name? ((name t) (namespace function-namespace))
  nil)

(defmethod valid-name? ((name symbol) (namespace function-namespace))
  (not (constantp name)))

(defmethod valid-name? ((name cons) (namespace function-namespace))
  (and (typep name '(cons (eql setf) (cons symbol null)))
       (valid-name? (second name) namespace)))

(defvar *variable-namespace* (make-instance 'env::eq-namespace))

(defvar *variable-information-namespace* (make-instance 'env::eq-namespace))

(defvar *tag-namespace* (make-instance 'env::eq-namespace))

(defclass global-environment (env:global-environment)
  ())

(defmethod initialize-instance :after ((instance global-environment) &key)
  (setf (env:lookup 'variable             'env:namespace instance) *variable-namespace*
        (env:lookup 'function             'env:namespace instance) *function-namespace*
        (env:lookup 'variable-information 'env:namespace instance) *variable-information-namespace*
        (env:lookup 'tag                  'env:namespace instance) *tag-namespace*))

(defvar *global-environment*
  (let ((env (make-instance 'global-environment)))
    (do-symbols (symbol '#:common-lisp)
      (when (and (fboundp symbol)
                 (not (macro-function symbol)))
        (when-let ((function (fdefinition symbol)))
          (setf (env:lookup symbol 'function env)
                (lambda (dynamic-environment &rest args)
                  (declare (ignore dynamic-environment))
                  (apply function args))))))
    (setf (env:lookup 'funcall 'function env)
          (lambda (dynamic-environment function &rest args)
            (apply function dynamic-environment args)))
    env))

(defclass dynamic-environment (env:lexical-environment)
  ())

(declaim (inline make-dynamic-environment))
(defun make-dynamic-environment (parent)
  (make-instance 'dynamic-environment :parent parent))

;;; Utilities

(declaim (inline variable-environment))
(defun variable-environment (name dynamic-environment lexical-environment)
  (cond ((not (env:lookup name 'variable-information lexical-environment
                          :if-does-not-exist nil))
         lexical-environment)
        ((functionp dynamic-environment)
         (funcall dynamic-environment))
        (t
         dynamic-environment)))

(defun variable-value (name dynamic-environment lexical-environment)
  (declare (type (not function) dynamic-environment))
  (let ((environment (variable-environment
                      name dynamic-environment lexical-environment)))
    (env:lookup name 'variable environment)))

(defun (setf variable-value) (new-value name dynamic-environment lexical-environment)
  (let ((environment (variable-environment
                      name dynamic-environment lexical-environment)))
    (setf (env:lookup name 'variable environment) new-value)))

(defun make-environment (parent)
  (make-instance 'env:lexical-environment :parent parent))

(defun evaluate-body-forms (forms dynamic-environment lexical-environment)
  (map nil (rcurry #'evaluate dynamic-environment lexical-environment)
       (butlast forms))
  (evaluate (lastcar forms) dynamic-environment lexical-environment))

(defun make-abstraction (lambda-list body static-environment)
  (lambda (dynamic-environment &rest args)
    (let ((environment             (make-environment static-environment))
          (new-dynamic-environment dynamic-environment))
      (flet ((make-new-dynamic-environment ()
               ;; Bind NEW-DYNAMIC-ENVIRONMENT to an actually new
               ;; dynamic environment when the first binding is
               ;; established.
               (if (eq new-dynamic-environment dynamic-environment)
                   (setf new-dynamic-environment
                         (make-dynamic-environment dynamic-environment))
                   new-dynamic-environment)))
        (loop :for name  :in lambda-list
              :for value :in args
              :do (setf (variable-value name #'make-new-dynamic-environment environment)
                        value)))
      (evaluate-body-forms body new-dynamic-environment environment))))

;;;

(defmethod evaluate ((form                symbol)
                     (dynamic-environment dynamic-environment)
                     (lexical-environment env:lexical-environment))
  (values (if (env:lookup form 'variable-information lexical-environment
                          :if-does-not-exist nil)
              (env:lookup form 'variable dynamic-environment)
              (env:lookup form 'variable lexical-environment))))

(defmethod evaluate ((form                t)
                     (dynamic-environment dynamic-environment)
                     (lexical-environment env:lexical-environment))
  form)

(defmethod evaluate ((form                cons)
                     (dynamic-environment dynamic-environment)
                     (lexical-environment env:lexical-environment))
  (evaluate-using-head (first form) form dynamic-environment lexical-environment))

(defmethod evaluate-using-head ((head                (eql 'if))
                                (form                t)
                                (dynamic-environment env:lexical-environment)
                                (lexical-environment env:lexical-environment))
  (destructuring-bind (test then else) (rest form)
    (if (evaluate test dynamic-environment lexical-environment)
        (evaluate then dynamic-environment lexical-environment)
        (evaluate else dynamic-environment lexical-environment))))

(defmethod evaluate-using-head ((head                (eql 'progn))
                                (form                t)
                                (dynamic-environment env:lexical-environment)
                                (lexical-environment env:lexical-environment))
  (evaluate-body-forms (rest form) dynamic-environment lexical-environment))

(defmethod evaluate-using-head ((head                (eql 'let))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (let ((lexical-environment     (make-environment lexical-environment))
        (new-dynamic-environment dynamic-environment))
    (flet ((make-new-dynamic-environment ()
             (if (eq new-dynamic-environment dynamic-environment)
                 (setf new-dynamic-environment (make-instance 'dynamic-environment :parent dynamic-environment))
                 new-dynamic-environment)))
      (destructuring-bind (bindings &rest forms) (rest form)
        (loop :for (name value-form) :in bindings
              :for value = (evaluate value-form dynamic-environment lexical-environment)
              :do (setf (variable-value name #'make-new-dynamic-environment lexical-environment)
                        value))
        (evaluate-body-forms forms new-dynamic-environment lexical-environment)))))

(defmethod evaluate-using-head ((head                (eql 'setq))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (destructuring-bind (name new-value-form) (rest form)
    (let* ((new-value   (evaluate new-value-form dynamic-environment lexical-environment))
           (environment (variable-environment name dynamic-environment lexical-environment)))
      (env:make-or-update name 'variable environment
                          (lambda ()
                            new-value)
                          (lambda (old-value container)
                            (declare (ignore old-value))
                            (values new-value t container))))))

(defmethod evaluate-using-head ((head                (eql 'labels))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (let ((lexical-environment (make-environment lexical-environment)))
    (destructuring-bind (bindings &rest forms) (rest form)
      (map nil (lambda (binding)
                 (destructuring-bind (name lambda-list &rest body) binding
                   (unless (valid-name? name *function-namespace*)
                     (error "~S is not a valid ~S name." name 'function))
                   (setf (env:lookup name 'function lexical-environment)
                         (make-abstraction lambda-list body lexical-environment))))
           bindings)
      (evaluate-body-forms forms dynamic-environment lexical-environment))))

(defmethod evaluate-using-head ((head                (eql 'function))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (env:lookup (second form) 'function lexical-environment))

(defmethod evaluate-using-head ((head                symbol)
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (let ((function  (env:lookup head 'function lexical-environment))
        (arguments (map 'list (rcurry #'evaluate dynamic-environment lexical-environment)
                        (rest form))))
    (apply function dynamic-environment arguments)))

(defmethod evaluate-using-head ((head                (eql 'quote))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (second form))

(defmethod evaluate-using-head ((head                (eql 'catch))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (destructuring-bind (tag &body body) (rest form)
    (let* ((tag                  (evaluate tag dynamic-environment lexical-environment))
           (continuation         (lambda (value)
                                   (return-from evaluate-using-head value)))
           (dynamic-environment  (env:augmented-environment
                                  dynamic-environment `((,tag . tag)) (list continuation))))
      (evaluate-body-forms body dynamic-environment lexical-environment))))

(defmethod evaluate-using-head ((head                (eql 'throw))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (destructuring-bind (tag value) (rest form)
    (let ((tag   (evaluate tag dynamic-environment lexical-environment))
          (value (evaluate value dynamic-environment lexical-environment)))
      (clouseau:inspect (list dynamic-environment lexical-environment))
      (funcall (env:lookup tag 'tag dynamic-environment) value))))

(defmethod evaluate-using-head ((head                (eql 'lambda))
                                (form                t)
                                (dynamic-environment dynamic-environment)
                                (lexical-environment env:lexical-environment))
  (destructuring-bind (lambda-list &body body) (rest form)
    (make-abstraction lambda-list body lexical-environment)))

;;; Test

(defun eval (form)
  (let ((dyn-env (make-instance 'dynamic-environment :parent *global-environment*))
        (lex-env (make-instance 'env:lexical-environment :parent *global-environment*)))
    (setf (env:lookup '*foo* 'variable-information lex-env) t)
    (evaluate form dyn-env lex-env)))

(assert (equal
         '(1 2 6 24)
         (eval '(labels ((fac (n)
                          (if (= n 0)
                              1
                              (* n (fac (1- n))))))
                 ;; (map 'list #'fac '(1 2 3 4 5 6))
                 (list (fac 1) (fac 2) (fac 3) (fac 4))))))

(assert (equal
         '(10 (8 2021) 2016 42)
         (eval `(labels ((get-foo ()
                           *foo*))
                  (let ((d 1000)
                        (*foo* 10))
                    (labels ((foo (a b c)
                               (setq d (+ d d))
                               (list a (+ b c d)))
                             (bar (e)
                               (lambda (f)
                                 (+ d e f))))
                      (list (get-foo)
                            (foo (+ (let ((a 1)) (progn (+ a 2)))
                                    (let ((a 5)) a))
                                 10 11)
                            (funcall (bar 7) 9)
                            (catch 'foo (funcall (lambda () (+ 42 (throw 'foo 42))))))))))))


(assert (equal
         '(1 (:foo 3) 1)
         (eval `(labels ((get-foo (*foo*)
                           (setq *foo* 3)
                           (bar))
                         (bar ()
                           (list ':foo *foo*)))
                  (let ((*foo* 1))
                    (list *foo* (get-foo 2) *foo*))))))

(assert (eql 2 (eval `(labels ((get-foo ()
                                 *foo*)
                               ((setf get-foo) (new-value)
                                 (setq *foo* new-value)))
                        (let ((*foo* 1))
                          (funcall #'(setf get-foo) 2)
                          *foo*)))))
