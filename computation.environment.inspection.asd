;;;; computation.environment.inspection.asd --- System definition for the computation.environment.inspection system
;;;;
;;;; Copyright (C) 2019, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "computation.environment.inspection"
  :description ""
  :license     "GPLv3"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("computation.environment"

                "mcclim"
                "clouseau")

  :components  ((:module "src/inspection"
                 :serial t
                 :components ((:file       "package")
                              (:file       "inspect")))))
