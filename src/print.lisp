;;;; print.lisp --- Printing and describing environment objects.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment)

;;; Describe

(defmethod describe-object ((object global-environment) stream)
  (pprint-logical-block (stream (list object))
    (format stream "~A~@:_~@:_" object)
    (describe-namespaces object stream)))

(defmethod describe-object ((object lexical-environment) stream)
  (pprint-logical-block (stream (list object))
    (format stream "~A~@:_~@:_" object)
    (describe-namespaces object stream)))

(defmethod describe-namespaces ((environment t) (stream t))
  (pprint-logical-block (stream (list environment) :per-line-prefix "  ")
    (let ((first? t))
      (map-entries
       (lambda (name namespace container)
         (declare (ignore container))
         (unless (eq name 'namespace)
           (if first?
               (setf first? nil)
               (format stream "~@:_~@:_"))
           (describe-namespace name namespace environment stream)))
       'namespace environment))))

(defmethod describe-namespace ((name        t)
                               (namespace   t)
                               (environment t)
                               (stream      t))
  (let ((*package* (find-package :keyword)))
    (format stream "~A ~S ~:D entr~:@P~@:_"
            (class-name (class-of namespace))
            name
            (entry-count namespace environment)))
  (pprint-logical-block (stream (list namespace) :per-line-prefix "  ")
    (multiple-value-bind (entries truncated? name-width)
        (entries-for-describe namespace environment)
      (loop :for (name value . properties) :in entries
            :for first? = t :then nil
            :do (unless first?
                  (pprint-newline :mandatory stream))
                (apply #'describe-binding name value namespace environment stream
                       :name-width name-width properties)
            :finally (when truncated?
                       (pprint-newline :mandatory stream)
                       (write-string ".." stream))))))

(defmethod entries-for-describe ((namespace   t)
                                 (environment t)
                                 &key
                                 (count *print-length*))
  (let ((result      '())
        (entry-count 0)
        (name-width  0))
    (map-entries (lambda (name value scope)
                   (declare (ignore scope))
                   (when (eql entry-count count)
                     (return-from entries-for-describe
                       (values result nil name-width)))
                   (maxf name-width (length (string name)))
                   (push (list name value) result)
                   (incf entry-count))
                 namespace environment)
    (values (nreverse result) nil name-width)))

(defmethod entries-for-describe ((namespace   t)
                                 (environment hierarchical-environment-mixin)
                                 &key
                                 (count *print-length*))
  (let ((result      '())
        (entry-count 0)
        (name-width  0))
    (map-entries (lambda (name value scope)
                   (when (eql entry-count count)
                     (return-from entries-for-describe
                       (values result nil name-width)))
                   (maxf name-width (length (typecase name ; TODO
                                              ((or character string symbol) (string name))
                                              (t (princ-to-string name)))))
                   (let ((inherited? (when (not (eq scope environment))
                                       scope)))
                     (push (list name value :inherited? inherited?) result))
                   (incf entry-count))
                 namespace environment)
    #+no (when-let ((parent (parent environment)))
           (map-entries (lambda (name value)
                          (when (eql entry-count count)
                            (return-from entries-for-describe
                              (values result nil name-width)))
                          (maxf name-width (length (string name)))
                          (push (list name value :inherited? parent) result)
                          (incf entry-count))
                        namespace parent))
    (values (nreverse result) nil name-width)))

(defmethod describe-binding ((name        t)
                             (value       t)
                             (namespace   t)
                             (environment t)
                             (stream      t)
                             &key
                             name-width
                             inherited?)
  (format stream "~VS → ~S~@[ [inherited from ~A]~]"
          name-width name value inherited?))
