;;;; protocol.lisp --- Tests for the protocol functions provided by the computation.environment system
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.protocol
  :in :computation.environment)

(test lookup.smoke
  "Smoke test for the `lookup' function."

  (let ((env (make-populated-global-environment)))
    (signals entry-does-not-exist-error
      (lookup 'foo 'function env))

    (is (equal (values nil nil)
               (lookup 'foo 'function env :if-does-not-exist nil)))

    (is (equal (values :bar t) (lookup 'bar 'function env)))))
