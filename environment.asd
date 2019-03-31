(defsystem "environment"
  :license     "GPLv3"

  :depends-on  ("alexandria"

                "utilities.print-items")

  :components  ((:module "src"
                 :serial t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "mixins")

                              (:file       "global-environment")
                              (:file       "lexical-environment")

                              (:file       "augment"))))
  :in-order-to ((test-op (test-op "environment/test"))))

(defsystem "environment/test"
  :depends-on  ("environment"

                "fiveam")

  :components  ((:module     "test"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "lexical-environment"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:environment.test '#:run-tests)))
