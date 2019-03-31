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

                              (:file       "augment")))))
