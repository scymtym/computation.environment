;;;; augment.lisp --- Tests for the environment augmentation functions
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.test)

(def-suite* :computation.environment.augment
  :in :computation.environment)

(test augment-environment!.smoke
  "Smoke test for the `augment-environment!' function."

  (let ((environment (make-populated-global-environment)))
    (env:augment-environment! environment '((foo . function)) '(:foo))
    (is (eq :bar (env:lookup 'bar 'function environment)))
    (is (eq :foo (env:lookup 'foo 'function environment)))))

(test augmented-environment.smoke
  "Smoke test for the `augmented-environment' function."

  (let* ((parent (make-populated-global-environment))
         (child  (env:augmented-environment parent '((foo . function)) '(:foo)
                                            :class 'env:lexical-environment)))
    (is (not (eq child parent)))

    (is (eq :bar (env:lookup 'bar 'function parent)))
    (is (eq nil  (env:lookup 'foo 'function parent :if-does-not-exist nil)))

    (is (eq :bar (env:lookup 'bar 'function child)))
    (is (eq :foo (env:lookup 'foo 'function child)))))

(test augment-namespace!.smoke
  "Smoke test for the `augment-namespace!' function."

  (let ((environment (make-populated-global-environment)))
    (env:augment-namespace! environment 'function '(foo) '(:foo))
    (is (eq :bar (env:lookup 'bar 'function environment)))
    (is (eq :foo (env:lookup 'foo 'function environment)))))

(test augmented-namespace.smoke
  "Smoke test for the `augmented-namespace' function."

  (let* ((parent (make-populated-global-environment))
         (child  (env:augmented-namespace parent 'function '(foo) '(:foo)
                                          :class 'env:lexical-environment)))
    (is (not (eq child parent)))

    (is (eq :bar (env:lookup 'bar 'function parent)))
    (is (eq nil  (env:lookup 'foo 'function parent :if-does-not-exist nil)))

    (is (eq :bar (env:lookup 'bar 'function child)))
    (is (eq :foo (env:lookup 'foo 'function child)))))
