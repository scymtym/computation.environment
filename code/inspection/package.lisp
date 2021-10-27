;;;; package.lisp --- Package definition for the inspection module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:computation.environment.inspection
  (:use
   #:cl)

  (:local-nicknames
   (#:a      #:alexandria)

   (#:env #:computation.environment)))
