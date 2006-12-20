;;; -*- Lisp -*- mode
(defpackage #:org.iodb.paren-psos-system
  (:use #:cl #:asdf))
(in-package :org.iodb.paren-psos-system)

(defsystem :paren-psos
  :description "ParenScript Object System - A CLOS-like object system for ParenScript."
  :version "0.1.0"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "lisp-src"
                        :components ((:file "packages")
				     (:file "psos-macrology" :depends-on ("packages"))
                                     )))
  :depends-on ("parenscript"))
