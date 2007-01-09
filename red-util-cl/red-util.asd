;;; -*- Lisp -*- mode                                                                                                       

(defpackage org.iodb.red-util-system
  (:use #:cl #:asdf))
(in-package :org.iodb.red-util-system)

(defsystem :red-util
  :description "Some utilities I use sometimes."
  :version "0.2.0"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT license"
  :components ((:file "util")))



