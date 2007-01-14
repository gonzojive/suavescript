(in-package org.iodb.paren-psos)

(defvar *include-documentation* nil)

(defun parse-direct-slot (slot-name &key reader writer accessor documentation)
  "Parses a direct-slot form and returns a psos-direct-slot-definition.
This doesn't handle multiple readers and writers."
  (declare (ignore accessor) (ignore writer) (ignore reader))
  (make-instance 'psos-direct-slot-definition
		 :name slot-name
		 :documentation documentation))

(defun parse-class-options (options)
  options)

(js:defjsmacro defclass (class-name &optional superclasses direct-slots &rest options)
  (expand-psos-definition
   (let ((psos-class
	  (make-instance 'psos-class-definition
			 :name class-name
			 :options (parse-class-options options)
			 :direct-superclasses superclasses
			 :direct-slot-definitions
			 (mapcar #'(lambda (dslot)
				     (apply #'parse-direct-slot dslot))
				 direct-slots))))
     (mapc #'(lambda (dslot) (setf (psos-slot-class dslot)
				   psos-class))
	   (classdef-direct-slot-definitions psos-class))
     psos-class)))

(js:defjsmacro defaultf (place default-val)
  `(setf ,place (or ,place ,default-val)))

(js:defjsmacro defgeneric (formal-name &body options)
  (declare (ignore options))
  (let ((name-as-string (string-downcase (string formal-name))))
    `(defvar ,formal-name (ensure-generic ,formal-name
			   (create :name ,name-as-string)))))

(defun parse-defjsmethod-args (args)
  (let* ((name (first args))
	 (qualifiers? (keywordp (second args)))
	 (qualifiers (if qualifiers? (list (second args)) nil))
	 (post-qualifers (if qualifiers? (cddr args) (cdr args)))
	 (lambda-list (first post-qualifers))
	 (body (rest post-qualifers)))
    (multiple-value-bind (requireds optionals rest? rest keys? keys)
	(paren-psos::parse-lambda-list lambda-list)
      (labels ((specializer-part (arg-form)
		 (if (listp arg-form) (second arg-form) nil))
	       (name-part (arg-form)
		 (if (listp arg-form) (first arg-form) arg-form)))
	(let ((specializers (mapcar #'specializer-part requireds))
	      (normal-lambda-list
	       (append (mapcar #'name-part requireds)
		       (when optionals
			 (apply #'list '&optional optionals))
		       (when keys?
			 (apply #'list '&keys keys))
		       (when rest?
			 (list '&rest rest)))))
	  (values name qualifiers specializers
		  normal-lambda-list body))))))

(js:defjsmacro defmethod (&rest args)
  (multiple-value-bind (formal-name method-qualifiers specializers argument-list body)
      (parse-defjsmethod-args args)
    (let ((name-as-string (string-downcase (string formal-name))))
      `(progn
	(defvar ,formal-name (ensure-generic ,formal-name (create :name ,name-as-string)))
	(ensure-method ,formal-name (array ,@specializers)
	 (lambda2 ,argument-list
	  ,@body)
	 ,(js:js-to-string (or (first method-qualifiers)
			       :primary)))))))

(js:defjsmacro call-next-method (&rest args)
  `(this.call-following-method ,@args))