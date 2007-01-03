(in-package org.iodb.paren-psos)

;;; This File Includes Utilities for sending CLOS instances across the net as
;;; RJSON
(defgeneric represent-rjson-slot (object slot  &optional seed)
  (:documentation "Should return two parenscript forms, one for
the key to the slot and the other for the value of the slot.
Return type expected is (slot-name . slot-value). Returns nil
if the slot should not be included."))

(defmethod represent-slot ((object standard-object) slot  &optional seed)
  (declare (ignore seed))
  (cons (js:js-to-string (slot-definition-name slot))
	(represent (if (slot-boundp-using-class (class-of object) object slot)
		       (slot-value-using-class (class-of object) object slot)
		       'undefined))))

(defmethod represent-rjson ((object structure-object) &optional seed)
  "Represents an object's slots and such in an RJSON-compatible way."
  'undefined)

(defmethod represent-rjson ((object standard-object) &optional seed)
  "Represents an object's slots and such in an RJSON-compatible way."
  (let ((create-args (list))
	(class (class-of object)))
    (setf create-args
	   (mapcan #'(lambda (slot-representation)
		       (list (car slot-representation) (cdr slot-representation)))
		   (remove-if #'null (mapcar #'(lambda (slot)
						 (represent-slot object slot seed))
					     (class-slots class)))))
    `(xtype
      ,(js:js-to-string (class-name class))
      ,(apply #'list 'create create-args))))