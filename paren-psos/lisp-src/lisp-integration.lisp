(in-package org.iodb.paren-psos)
;;; This File Includes Utilities For Integrating Clos Classes And PSOS classes
;;; CLOS and PSOS are not identical systems, and PSOS is not refined, so beware

;; to be specific, CLOS expresses more in first-order objects than does PSOS
;; generic functions, methods, and method combinations will probably be supported
;; as first-order objects in the PSOS runtime in the future because they are already
;; expressed in the runtime

(defvar *include-documentation* nil)

(defclass compilation-session ()
  ()
  (:documentation "Contains information about a compilation session for parenscript.
The compiler can keep track of information that links parenscript to a lisp session."))

(defclass psos-class-definition ()
  ((name :accessor classdef-name :initarg :name
	 :documentation "Name of class definition as a symbol")
   (direct-superclasses :accessor classdef-direct-superclasses :initarg :direct-superclasses
			:documentation "A list of psos-class designators, either symbols or class definitions.")
   (direct-slot-definitions :accessor classdef-direct-slot-definitions :initarg :direct-slot-definitions)
   (options :accessor classdef-options :initarg :options
	    :documentation "options attached to the class definition")
   (documentation :accessor classdef-documentation :initarg :documentation :initform nil))
  (:documentation "A class definition in the parenscript object system."))

(defgeneric expand-psos-definition (psos-definition)
  (:documentation "This is poorly named."))

(defmethod expand-psos-definition ((class-definition psos-class-definition))
  "Expands a class definition to a parenscript form."
  (with-accessors ((class-name classdef-name) (superclasses classdef-direct-superclasses)
		   (slot-definitions classdef-direct-slot-definitions)
		   (options classdef-options)
		   (documentation classdef-documentation))
    class-definition
    `(progn
      (defvar ,class-name (ensure-class
			   ,class-name
			   (create
			    :name ,(string-downcase (string class-name))
			    :direct-superclasses ,(if (null superclasses)
						      nil
						      `(array ,@superclasses))
			    :slot-definitions nil	 ; place-holder for class slots
			    ,@(apply #'append options))))
      ,@(mapcar #'expand-psos-definition slot-definitions))))
  

(defclass psos-direct-slot-definition ()
  ((name :accessor psos-slot-name :initarg :name
	 :documentation "Name of class slot definition as a symbol")
   (class :accessor psos-slot-class :initarg :class
	 :documentation "class definition associated with this slot.")
   (readers :accessor psos-slot-readers :initarg :readers :initform nil
	    :documentation "readers asssociated with this slot.")
   (writers :accessor psos-slot-writers :initarg :writers :initform nil
	    :documentation "writers asssociated with this slot.")
   (documentation :accessor psos-slot-documentation :initarg :documentation :initform nil))
  (:documentation "A class definition in the parenscript object system."))

(defun js-format-symbol (format-string &rest format-args)
  (intern (string-upcase
	   (apply #'format nil format-string format-args))))

;(defun expand-psos-slot-definition (slot-

(defmethod expand-psos-definition ((slot-definition psos-direct-slot-definition))
  (let ((accessors-output-convention :java))
    (with-accessors ((slot-name psos-slot-name) (slot-class psos-slot-class)
		     (readers psos-slot-readers) (writers psos-slot-writers)
		     (documentation psos-slot-documentation))
      slot-definition
      (case accessors-output-convention
	((or :clos :lisp) ;this is not implemented because setf is strange in parenscript
	 `(progn
	   ,@(mapcar #'(lambda (reader)
			 `(defmethod ,slot-name ((obj hello-replace))
			   (return (slot-value obj ,slot-name))))
		     readers)))
	(:java
	 (let ((slot-form
		`(slot-value obj
		  (quote ,(js-format-symbol "~A" slot-name)))))
	   `(progn
	     (defmethod ,(js-format-symbol "get-~A" slot-name)
		 ((obj ,(classdef-name slot-class)))
	       (return ,slot-form))
	     (defmethod ,(js-format-symbol "set-~A" slot-name)
		 ((obj ,(classdef-name slot-class)) value)
	       (return (setf ,slot-form value)))) ))))))


(defclass psos-class-import ()
  ((imported-class :accessor imported-class :initarg :imported-class)
   (alias :accessor psos-import-alias :initarg :alias
	  :documentation "Symbol of the clos class mapped to the lisp imported-class"))
   (:documentation "A class import made by a some parenscript. NOT USED RIGHT NOW"))

(defgeneric import-lisp-class (lisp-class &key alias)
  (:documentation "Returns a psos-class-definition corresponding to the given lisp class."))
(defgeneric import-lisp-slot-definition (slot-definition)
  (:documentation "Returns a psos-direct-slot-definition corresponding to the given lisp slot definition."))

(defmethod import-lisp-class ((lisp-class standard-class) &key alias)
  (let ((class-definition
	 (make-instance 'psos-class-definition
			:name (or alias (class-name lisp-class))
			:options nil
			:direct-superclasses (mapcar #'(lambda (class) (class-name class))
						     (class-direct-superclasses lisp-class))
			:direct-slot-definitions (mapcar #'import-lisp-slot-definition
							 (class-direct-slots lisp-class))
			:documentation (documentation lisp-class t))))
    (mapc #'(lambda (dslot) (setf (psos-slot-class dslot)
				  class-definition))
	  (classdef-direct-slot-definitions class-definition))
    class-definition))

(defmethod import-lisp-slot-definition ((slot-definition standard-direct-slot-definition))
  (make-instance 'psos-direct-slot-definition
		  :name (slot-definition-name slot-definition)
		  :class nil
		  :readers (slot-definition-readers slot-definition)
		  :writers nil
		  :documentation (documentation slot-definition t)))

(js:defjsmacro import-class (class-name &key alias)
  (expand-psos-definition
   (import-lisp-class (find-class class-name) :alias alias)))