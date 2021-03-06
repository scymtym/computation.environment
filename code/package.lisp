;;;; package.lisp --- Package definition for the computation.environment system
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:computation.environment
  (:use
   #:cl
   #:alexandria)

  ;; Conditions
  (:export
   #:name

   #:namespace-does-not-exist-error

   #:namespace

   #:entry-does-not-exist-error)

  ;; Bindings protocol
  (:export
   #:make-bindings
   #:entry-count-in-bindings
   #:map-entries-in-bindings
   #:lookup-in-bindings)                        ; also setf

  ;; Environment protocol
  (:export
   #:entry-count    #:entry-count-using-scope
   #:map-entries    #:map-entries-using-scope
   #:entries        #:entries-using-scope

   #:lookup         #:lookup-using-scope        ; also setf

   #:make-or-update
   #:ensure)

  ;; Hierarchical environment protocol
  (:export
   #:parent
   #:root
   #:depth)

  ;; Class standard-environment
  (:export
   #:standard-environment)

  ;; Class global-environment
  (:export
   #:global-environment)

  ;; Class lexical-environment
  (:export
   #:lexical-environment)

  ;; Augmentation
  (:export
   #:augment-environment!
   #:augmented-environment

   #:augment-namespace!
   #:augmented-namespace)

  (:documentation
   "Protocols and classes for representing environments."))
