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

(js:defjsmacro defgeneric (formal-name)
  `(defvar ,formal-name (ensure-generic ,formal-name ,(js:js-to-string formal-name))))

(defun parse-defjsmethod-args (args)
  "Parses defmethod arguments, which fit the grammar
(name method-qualifier? specialized-argument-list body-forms)"
  (let ((fn-name (first args)) (qualifiers nil)	(lambda-list nil)
	(specializers nil)     (body nil))
    (if (keywordp (second args))
	(progn
	  (push (second args) qualifiers)
	  (setf lambda-list (third args))
	  (setf body (subseq args 3)))
	(progn
	  (setf lambda-list (second args))
	  (setf body (subseq args 2))))
    (setf specializers
	  (mapcar #'(lambda (arg)
		      (if (listp arg)
			  (second arg)
			  nil))
	    lambda-list))
    (setf lambda-list
	  (mapcar #'(lambda (arg)
		      (if (listp arg)
			  (first arg)
			  arg))
	    lambda-list))
    (values fn-name qualifiers specializers lambda-list body)))
    
(js:defjsmacro defmethod (&rest args)
  (multiple-value-bind (formal-name method-qualifiers specializers argument-list body)
      (parse-defjsmethod-args args)
    `(progn
      (defvar ,formal-name (ensure-generic ,formal-name ,(js:js-to-string formal-name)))
      (create-method
      ,formal-name
      (array ,@specializers)
      (lambda ,argument-list
	,@body)
      ,(js:js-to-string (or (first method-qualifiers)
			    :primary))))))

(js:defjsmacro call-next-method (&rest args)
  `(this.call-following-method ,@args))

(js:defjsmacro doelements (heading &rest body)
  (let ((varname (first heading)))
    `(dolist ,heading
      (if (not (slot-value ,varname 'tag-name))
	  (continue))
      ,@body)))

(js:defjsmacro def-rdf-aliases (&rest aliases)
  (let ((result `(progn)))
    (dolist (alias aliases)
      (push `(def-rdf-alias ,alias) result))
    (nreverse result)))
    
(js:defjsmacro def-rdf-alias (alias &optional str)
  (let ((resource-id-str (concatenate
			  'string
			  "#"
			  (js:js-to-string
			   (intern
			    (concatenate 'string "-" (string alias))))))
	(resource-id-str2 (concatenate
			   'string
			   "#"
			   (js:js-to-string alias))))
    `(defvar ,alias ,(if str
			 `(lookup-rdf-resource ,(concatenate 'string "#" str))
			 `(or (lookup-rdf-resource ,resource-id-str)
			   (lookup-rdf-resource ,resource-id-str2))))))
		     
  
(js:defjsmacro defrdfclass (class-name rdf-tag &rest args)
  `(progn
    (defjsclass ,class-name ,@args)
    (set-rdf-resource ,rdf-tag ,class-name)))
