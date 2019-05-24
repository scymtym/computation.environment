;;;; computation.environment.asd --- System definition for the computation.environment system
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "computation.environment"
  :description "A generic and extensible (and slow) implementation of environments"
  :license     "GPLv3"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("closer-mop"
                "alexandria"

                "utilities.print-items")

  :components  ((:module "src"
                 :serial t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "namespace-mixins")
                              (:file       "mixins") ; TODO environment-mixins

                              (:file       "global-environment")
                              (:file       "lexical-environment")

                              (:file       "augment"))))
  :in-order-to ((test-op (test-op "computation.environment/test"))))

(defsystem "computation.environment/test"
  :depends-on  ("computation.environment"

                "fiveam")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "lexical-environment")

                              (:file       "augment"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:computation.environment.test '#:run-tests)))
