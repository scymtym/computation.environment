(cl:defpackage #:environment
  (:use
   #:cl
   #:alexandria)

  ;; Environment protocol
  (:export
   #:entries

   #:lookup                             ; also setf

   #:make-or-update
   #:ensure)

  ;; Hierarchical environment protocol
  (:export
   #:parent

   #:direct-entries

   #:direct-lookup)

  ;; Class standard-environment
  (:export
   #:standard-environment)

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
   "TODO"))
