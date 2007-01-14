;;; -*- Lisp -*- mode
(defpackage org.iodb.red-browser-system
  (:use #:cl #:asdf))
(in-package :org.iodb.red-browser-system)
 
(defsystem :red-browser
  :description "red-browser is a slightly more advanced http client with more signals and
restarts, and maybe caching sometime in the future."
  :version "0.0.1"
  :author "Red Daly <reddaly at gmail>"
  :license "MIT License"
  :components ((:module	"src"
			:components ((:file "package")
				     (:file "red-browser" :depends-on ("package"))
				     )))
  :depends-on ("trivial-http"))
