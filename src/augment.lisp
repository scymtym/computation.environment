(cl:in-package #:environment)

;;; Augmenting arbitrary namespaces

(defun augment-environment! (environment names values)
  (loop :for (name . namespace) :in names
        :for value              :in values
        :do (setf (lookup name namespace environment) value))
  environment)

(defun augmented-environment (parent names values
                              &key (class (class-of parent)))
  (let ((environment (make-instance class :parent parent)))
    (augment-environment! environment names values)
    environment))

;;; Augmenting a single namespace

(defun augment-namespace! (environment namespace names values)
  (loop :for name  :in names
        :for value :in values
        :do (setf (lookup name namespace environment) value))
  environment)

(defun augmented-namespace (parent namespace names values
                            &key (class (class-of parent)))
  (let ((environment (make-instance class :parent parent)))
    (augment-namespace! environment namespace names values)
    environment))
