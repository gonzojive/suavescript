;;; -*- Lisp -*- mode
(defpackage org.iodb.cl-sails-system
  (:use #:cl #:asdf))
(in-package :org.iodb.cl-sails-system)

(defsystem cl-sails
  :description "A lisp and parenscript implementation of Suave sails."
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail>"
  :license "GPL version 2: http://www.gnu.org/licenses/gpl.html"
  :components ((:module "lisp-src"
			:components ((:file "package")
				     (:file "parser" :depends-on ("package")))))
  :depends-on ("cl-ppcre" "xml-mop" "parenscript"))
