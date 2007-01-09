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
				     (:file "rjson" :depends-on ("packages"))
				     (:file "lisp-integration" :depends-on ("rjson"))
				     (:file "net-transmit" :depends-on ("lisp-integration"))
				     (:file "parse-lambda-list" :depends-on ("packages"))
				     (:file "util-macrology" :depends-on ("parse-lambda-list"))
				     (:file "psos-macrology" :depends-on ("util-macrology" "lisp-integration"))
                                     )))
  :depends-on ("parenscript" "closer-mop" "json"))
