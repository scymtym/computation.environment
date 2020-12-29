;;;; protocol.lisp --- Tests for the protocol functions provided by the computation.environment system
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.protocol
  :in :computation.environment)

(test entry-count.smoke
  "Smoke test for the `entry-count' function."

  (let ((environment (make-empty-global-environment)))
    (is (eql 3 (entry-count 'namespace environment)))
    (is (eql 0 (entry-count 'variable  environment)))
    (is (eql 0 (entry-count 'function  environment))))

  (let ((environment (make-populated-global-environment)))
    (is (eql 3 (entry-count 'namespace environment)))
    (is (eql 0 (entry-count 'variable  environment)))
    (is (eql 1 (entry-count 'function  environment)))))

(test lookup.smoke
  "Smoke test for the `lookup' function."

  (let ((environment (make-populated-global-environment)))
    (mapc (lambda (arguments-and-expected)
            (destructuring-bind
                ((name namespace &rest more-arguments)
                 expected-value &optional expected-value? expected-container)
                arguments-and-expected
              (flet ((do-it ()
                       (apply #'lookup name namespace environment
                              more-arguments)))
                (case expected-value
                  (entry-does-not-exist-error
                   (signals entry-does-not-exist-error (do-it)))
                  (namespace-does-not-exist-error
                   (signals namespace-does-not-exist-error (do-it)))
                  (t
                   (multiple-value-bind (value value? container) (do-it)
                     (is (eql expected-value     value))
                     (is (eq  expected-value?    value?))
                     (is (eq  expected-container container))))))))
          `(;; Namespace does not exist
            ((foo foo)                             namespace-does-not-exist-error)
            ((foo foo :if-does-not-exist nil)      namespace-does-not-exist-error)
            ;; Name is not bound in namespace
            ((foo function)                        entry-does-not-exist-error)
            ((foo function :if-does-not-exist nil) nil  nil nil)
            ;; Name is bound in namespace
            ((bar function)                        :bar t   ,environment)))))

(test setf-lookup.smoke
  "Smoke test for the `(setf lookup)' function."

  (mapc (lambda (arguments-and-expected)
          (destructuring-bind
              ((new-value name namespace) expected-value)
              arguments-and-expected
            (let ((environment (make-populated-global-environment)))
              (flet ((do-it ()
                       (setf (lookup name namespace environment)
                             new-value)))
                (case expected-value
                  (entry-does-not-exist-error
                   (signals entry-does-not-exist-error (do-it)))
                  (namespace-does-not-exist-error
                   (signals namespace-does-not-exist-error (do-it)))
                  (t
                   (let ((result (do-it)))
                     (is (eql expected-value result)))
                   (multiple-value-bind (value value? container)
                       (lookup name namespace environment
                               :if-does-not-exist nil)
                     (is (eql expected-value value))
                     (is-true value?)
                     (is (eq environment     container)))))))))
        `(;; Namespace does not exist
          ((1 foo foo)      namespace-does-not-exist-error)
          ;; Name is not bound in namespace.
          ((1 foo function) 1)
          ;; Name is already bound in namespace.
          ((1 bar function) 1))))

(test make-or-update.smoke
  "Smoke test for the `make-or-update' function."

  (mapc (lambda (arguments-and-expected)
          (destructuring-bind
              ((name namespace) new-value (updated-value updated?)
               expected-new-value expected-new-value?
               expected-old-value expected-old-container)
              arguments-and-expected
            (let* ((environment            (make-populated-global-environment))
                   (expected-old-container (case expected-old-container
                                             (:environment
                                              environment)
                                             (t
                                              expected-old-container))))
              (flet ((do-it ()
                       (make-or-update name namespace environment
                                       (lambda ()
                                         new-value)
                                       (lambda (old-value old-container)
                                         (declare (ignore old-value old-container))
                                         (values updated-value updated?)))))
                (multiple-value-bind
                      (new-value new-value? old-value old-container)
                    (do-it)
                  (is (eql expected-new-value     new-value))
                  (is (eq  expected-new-value?    new-value?))
                  (is (eql expected-old-value     old-value))
                  (is (eq  expected-old-container old-container)))))))
        `(((foo function) :new-foo (:updated-foo t)   :new-foo     t   nil  nil)
          ((bar function) :new-bar (:updated-bar t)   :updated-bar t   :bar :environment)
          ((bar function) :new-bar (nil          nil) :bar         nil :bar :environment))))

(test ensure.smoke
  "Smoke test for the `ensure' function."

  (mapc (lambda (arguments-and-expected)
          (destructuring-bind
              ((name namespace) new-value
               expected-new-value expected-new-value? expected-old-container)
              arguments-and-expected
            (let* ((environment            (make-populated-global-environment))
                   (expected-old-container (case expected-old-container
                                             (:environment
                                              environment)
                                             (t
                                              expected-old-container))))
              (flet ((do-it ()
                       (ensure name namespace environment
                               (constantly new-value))))
                (multiple-value-bind (new-value new-value? old-container)
                    (do-it)
                  (is (eql expected-new-value     new-value))
                  (is (eq  expected-new-value?    new-value?))
                  (is (eq  expected-old-container old-container)))))))
        `(((foo function) :new-foo :new-foo t   nil)
          ((bar function) :new-bar :bar     nil :environment)
          ((bar function) :new-bar :bar     nil :environment))))
