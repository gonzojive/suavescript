;;; -*- lisp -*-(defpackage :xmlisp.system  (:use :cl :asdf))(in-package :xmlisp.system)(defsystem xmlisp    :components ((:file "package")                 (:file "xml-utilities" :depends-on ("package"))                 (:file "element-definition" :depends-on ("package"))                 (:file "xml-tag-definitions" :depends-on ("package"))                 (:file "element-implementation" :depends-on ("package" "element-definition"))                 (:file "name-character-check" :depends-on ("package"))                 (:file "xml-parser" :depends-on ("package" "xml-utilities" "element-definition"))                 (:file "xml-writer" :depends-on ("package" "xml-utilities" "element-definition"))                 (:file "XMLisp" :depends-on ("package" "xml-utilities" "element-definition" "name-character-check"                                                        "xml-parser" "xml-writer"))))