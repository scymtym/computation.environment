;;;; package.lisp --- Package definition for the visualization module.
;;;;
;;;; Copyright (C) 2019, 2020, 2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:computation.environment.visualization
  (:use
   #:cl)

  (:local-nicknames
   (#:a      #:alexandria)

   (#:env #:computation.environment)))
