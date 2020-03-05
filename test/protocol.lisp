;;;; protocol.lisp --- Tests for the protocol functions provided by the computation.environment system
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.protocol
  :in :computation.environment)

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
                  (t
                   (multiple-value-bind (value value? container) (do-it)
                     (is (eql expected-value     value))
                     (is (eq  expected-value?    value?))
                     (is (eq  expected-container container))))))))
          `(;; Namespace does not exist
            ((foo foo)                             entry-does-not-exist-error)
            ((foo foo :if-does-not-exist nil)      entry-does-not-exist-error)
            ;; Name is not bound in namespace
            ((foo function)                        entry-does-not-exist-error)
            ((foo function :if-does-not-exist nil) nil  nil nil)
            ;; Name is bound in namespace
            ((bar function)                        :bar t   ,environment)))))
