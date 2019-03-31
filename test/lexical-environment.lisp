(cl:in-package #:environment.test)

(def-suite* :environment.lexical-environment
  :in :environment)

(test construction.smoke
  "Test constructing `lexical-environment' instances."

  (signals error (make-instance 'lexical-environment)))
