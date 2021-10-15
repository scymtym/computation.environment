;;;; computation.environment.asd --- System definition for the computation.environment system
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "computation.environment"
  :description "A generic and extensible (and slow) implementation of environments"
  :license     "GPLv3"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("closer-mop"
                "alexandria"

                "utilities.print-items")

  :components  ((:module     "code"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "conditions")
                              (:file       "protocol")

                              (:file       "namespace-mixins")
                              (:file       "environment-mixins")

                              (:file       "global-environment")
                              (:file       "lexical-environment")

                              (:file       "augment")

                              (:file       "print"))))
  :in-order-to ((test-op (test-op "computation.environment/test"))))

(defsystem "computation.environment/examples"
  :depends-on  ("computation.environment")

  :components  ((:module     "examples"
                 :serial     t
                 :components ((:file       "defclass")
                              ; (:file       "interpreter")
                              ))))

(defsystem "computation.environment/test"
  :description "Tests for the computation.environment system."
  :license     "GPLv3"

  :depends-on  ("computation.environment"
                "computation.environment/examples"

                "fiveam")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "util")
                              (:file       "protocol")

                              (:file       "global-environment")
                              (:file       "lexical-environment")

                              (:file       "augment"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:computation.environment.test '#:run-tests)))
