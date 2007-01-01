(defpackage :org.iodb.amazon.ecs
  (:nicknames :amazon-ecs :ecs :iodb-ecs)
  (:use :common-lisp)
  (:export "+amazon-merchant-id+" "generate-ecs-url"))

(in-package :org.iodb.amazon.ecs)

;;used this to generate export list:
;(do-symbols (sym :org.iodb.amazon.ecs)
;       (if (eql (find-package :org.iodb.amazon.ecs) (symbol-package sym))
;;         (export sym)
;	   (format t "~A~%" sym)))
; sorted via (lambda (sym1 sym2) (string-lessp (string sym1) (string sym2))))
