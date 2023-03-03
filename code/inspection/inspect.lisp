;;;; inspect.lisp --- Inspection support for environment objects.
;;;;
;;;; Copyright (C) 2019-2023 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:computation.environment.inspection)

;;; `entry-place'

(defclass entry-place (clouseau:basic-place)
  ((%interned-cells :allocation :class
                    :reader     interned-cells
                    :initform   (make-hash-table :test     #'equal
                                                 :weakness :key-and-value))))

(defmethod shared-initialize :around ((instance   entry-place)
                                      (slot-names t)
                                      &rest args &key (cell nil cell-supplied?))
  (if cell-supplied?
      (apply #'call-next-method instance slot-names
             (list* :cell (a:ensure-gethash cell (interned-cells instance) cell)
                    (a:remove-from-plist args :cell)))
      (call-next-method)))

(defmethod clouseau:supportsp ((place     entry-place)
                               (operation (eql 'clouseau:remove-value)))
  t)

(defmethod clouseau:value ((place entry-place))
  (destructuring-bind (name namespace environment) (clouseau:cell place)
    (env:lookup name namespace environment)))

(defmethod (setf clouseau:value) ((new-value t) (place entry-place))
  (destructuring-bind (name namespace environment) (clouseau:cell place)
    (setf (env:lookup name namespace environment) new-value)))

(defmethod clouseau:remove-value ((place entry-place))
  ) ; TODO

;;; `inspected-environment'

(defclass inspected-environment (clouseau:inspected-instance)
  ())

(defmethod inspect-namespace? ((name      t)
                               (namespace t)
                               (object    t)
                               (state     inspected-environment))
  (not (eq name 'env:namespace)))

(defmethod clouseau:object-state-class ((object env:global-environment) (place t))
  'inspected-environment)

(defmethod clouseau:object-state-class ((object env:lexical-environment) (place t))
  'inspected-environment)

(defmethod clouseau:make-object-state ((object env:lexical-environment) (place t))
  (make-instance (clouseau:object-state-class object place) :place      place
                                                            :slot-style :entries))

;;; `inspected-namespace'

(defclass inspected-namespace (clouseau:inspected-instance
                               clouseau::remembered-collapsed-style-mixin)
  ((%name :initarg :name
          :reader  name)))

(defmethod clouseau:inspect-object-using-state ((object t)
                                                (state  inspected-namespace)
                                                (style  (eql :name-only))
                                                (stream t))
  (princ (name state) stream))

;;; `namespace-in-environment-place'

(defclass namespace-in-environment-place (clouseau:pseudo-place)
  ((%name :initarg  :name
          :accessor name)))

(defmethod clouseau:object-state-class ((object standard-object)
                                        (place  namespace-in-environment-place))
  'inspected-namespace)

(defmethod clouseau:make-object-state ((object t)
                                       (place  namespace-in-environment-place))
  (make-instance (clouseau:object-state-class object place)
                 :place place
                 :name  (name place)
                 :style :name-only))

;;; Inspection methods

(defun print-namespace-header (object name namespace stream)
  (write-string "Namespace " stream)
  ;; TODO `(namespace-in-environment-place :name ,name) in FORMATTING-PLACE
  (clouseau:formatting-place (object 'namespace-in-environment-place namespace
                                     nil present-object :place-var place)
    (setf (name place) name)
    (present-object stream))
  (write-char #\Space stream)
  (clouseau:badge stream "~D entr~:@P" (env:entry-count namespace object)))

(defun inspect-namespace (object name namespace stream)
  (clouseau:with-section (stream)
      (print-namespace-header object name namespace stream)

    (multiple-value-bind (entries truncated?)
        (env::entries-for-describe namespace object)
      (clouseau:with-placeholder-if-empty (stream)
        ((null entries)
         (write-string "no entries" stream)
         (terpri stream))          ; TODO why terpri?
        (t
         (clouseau:with-preserved-cursor-x (stream)
           (clim:formatting-table (stream)
             (loop :with cells = (interned-cells (c2mop:class-prototype ; TODO
                                                  (c2mop:ensure-finalized
                                                   (find-class 'entry-place))))
                   :for (name value . properties) :in entries
                   :for inherited = (getf properties :inherited?)
                   :for cell = (list name namespace (or inherited object))
                   :do (clouseau:with-style (stream (if inherited :unbound nil))
                         (clim:formatting-row (stream)
                           (clouseau:formatting-place
                               (object 'clouseau:pseudo-place name nil present-object)
                             (clim:formatting-cell (stream)
                               (present-object stream)))
                           (clouseau:formatting-place (object 'entry-place (a:ensure-gethash cell cells cell)
                                                              present-place present-object)
                             (clim:formatting-cell (stream)
                               (present-place stream))
                             (clim:formatting-cell (stream)
                               (present-object stream))
                             (when inherited
                               (clim:formatting-cell (stream)
                                 (format stream "inherited from ~A" inherited)))))))))
         (when truncated?
           (clouseau:with-style (stream :note)
             (write-string "? entries omitted" stream))))))))

(defun inspect-all-namespaces (object state stream)
  (env:map-entries
   (lambda (name namespace environment)
     (declare (ignore environment))
     (when (inspect-namespace? name namespace object state)
       (inspect-namespace object name namespace stream)))
   'env:namespace object))

(defmethod clouseau:inspect-object-using-state ((object t)
                                                (state  inspected-environment)
                                                (style  (eql :expanded-body))
                                                (stream t))
  ;; Parent environment.
  (clouseau:with-preserved-cursor-x (stream)
    (clim:formatting-table (stream)
      (clouseau:format-place-row stream object 'clouseau:reader-place 'env:parent
                                 :label "Parent")))
  ;; Namespaces and entries in OBJECT.
  (case (clouseau::slot-style state)
    (:entries
     (inspect-all-namespaces object state stream))
    (t
     (call-next-method))))

;;; Commands

(clim:define-command (com-slot-style-entries :command-table clouseau:inspector-command-table
                                             :name          "Show entries instead of slots")
    ((object 'inspected-environment
      :gesture (:select
                :tester ((object)
                         (not (eq (clouseau::slot-style object) :entries)))
                :documentation "Show entries instead of slots")))
  (setf (clouseau::slot-style object) :entries))
