(defpackage org.iodb.paren-psos
  (:nicknames paren-psos)
  (:documentation "A CLOS-like object system for ParenScript.")
  (:use :cl :closer-mop :json)
  (:export :encode-rjson-string))

(in-package org.iodb.paren-psos)

