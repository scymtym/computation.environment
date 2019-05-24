;;;; augment.lisp --- Tests for the environment augmentation functions
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(defun make-convenient-environment ()
  (let ((env (make-instance 'global-environment)))
    (setf (lookup 'function 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace)
          (lookup 'variable 'computation.environment::namespace env)
          (make-instance 'computation.environment::eq-namespace))
    (setf (lookup 'bar 'function env) :bar)
    env))

(def-suite* :computation.environment.augment
  :in :computation.environment)

(test augment-environment!.smoke
  "Smoke test for the `augment-environment!' function."

  (let ((environment (make-convenient-environment)))
    (augment-environment! environment '((foo . function)) '(:foo))
    (is (eq :bar (lookup 'bar 'function environment)))
    (is (eq :foo (lookup 'foo 'function environment)))))

(test augmented-environment.smoke
  "Smoke test for the `augmented-environment' function."

  (let* ((parent (make-convenient-environment))
         (child  (augmented-environment parent '((foo . function)) '(:foo)
                                        :class 'lexical-environment)))
    (is (not (eq child parent)))

    (is (eq :bar (lookup 'bar 'function parent)))
    (is (eq nil  (lookup 'foo 'function parent :if-does-not-exist nil)))

    (is (eq :bar (lookup 'bar 'function child)))
    (is (eq :foo (lookup 'foo 'function child)))))

(test augment-namespace!.smoke
  "Smoke test for the `augment-namespace!' function."

  (let ((environment (make-convenient-environment)))
    (augment-namespace! environment 'function '(foo) '(:foo))
    (is (eq :bar (lookup 'bar 'function environment)))
    (is (eq :foo (lookup 'foo 'function environment)))))

(test augmented-namespace.smoke
  "Smoke test for the `augmented-namespace' function."

  (let* ((parent (make-convenient-environment))
         (child  (augmented-namespace parent 'function '(foo) '(:foo)
                                      :class 'lexical-environment)))
    (is (not (eq child parent)))

    (is (eq :bar (lookup 'bar 'function parent)))
    (is (eq nil  (lookup 'foo 'function parent :if-does-not-exist nil)))

    (is (eq :bar (lookup 'bar 'function child)))
    (is (eq :foo (lookup 'foo 'function child)))))
