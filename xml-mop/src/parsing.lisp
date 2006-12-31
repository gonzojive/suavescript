(in-package :org.iodb.xml-mop)

(defgeneric process-element (parent-element tag-name attributes)
  (:documentation "Adds the child element with the given tag name and
 attribs to the parent elem."))

(defgeneric find-allowed-element (parent-element tag-name)
  (:documentation "Finds the element class underneath parent-element that
corresponds to the tag string."))

(defun descriptor-matches? (descriptor test-string)
  (let* ((matcher-string (descriptor-matcher descriptor))
	 (result
	  (if (descriptor-case-sensitive descriptor)
	      (string-equal matcher-string test-string)
	      (string-equal (string-upcase matcher-string)
			    (string-upcase test-string)))))
      result))

(defmethod find-allowed-element ((parent-element element) tag-name)
  (find-allowed-element (class-of parent-element) tag-name))

(defun find-element-class-matching-tag (allowed-element-class tag-name)
  (labels ((find-matching-subclass (element-class)
	     (format t "Searching class ~A for a matching tag descriptor~%" element-class) 
	     (or (some #'(lambda (descriptor)
			   (and (descriptor-matches? descriptor tag-name)
				element-class))
		       (element-class-tag-descriptors element-class))
		 (some #'find-matching-subclass
		       (class-direct-subclasses element-class)))))
    (find-matching-subclass allowed-element-class)))

(defgeneric find-subelement-matching-tag (parent-element tag-name)
  (:documentation "Finds the most specific sub-element class of the supplied parent
element (generally an element class) that matches the tag string.  This will
check the slot definitions of the class for a slot with matching 'subelement'
descriptor, and then check the allowed-subelements field of the class."))


(defgeneric find-slot-matching-subelement (element tag-name)
  (:documentation "Finds a slot in an element class that matches the tag-name given"))
(defmethod find-slot-matching-subelement ((parent-element-class t) tag-name)
  nil)
(defmethod find-slot-matching-subelement ((parent-element-class element-class) tag-name)
  (values-list
   (some
    #'(lambda (slot-definition)
	(some #'(lambda (slot-subelement-descriptor)
		  (when (descriptor-matches? slot-subelement-descriptor tag-name)
		    (list slot-definition 
			  (find-class (descriptor-element-type slot-subelement-descriptor)))))
	      (element-slot-subelements slot-definition)))
    (class-slots parent-element-class))))

(defmethod find-subelement-matching-tag ((parent-element-class element-class) tag-name)
  (values-list
   (reverse
    (some #'(lambda (test-element-class)
	      ; look through all the superclasses of our element-class to
	      ; see if we find a match anywhere in the inheritance chain

	      (multiple-value-list ; should return (slot-def element-class)
	       (find-slot-matching-subelement test-element-class tag-name)))
	  (class-precedence-list parent-element-class)))))
  
(defmethod find-allowed-element ((parent-element-class xml-treenode-class) tag-name)
  (some 
   #'(lambda (test-element-class)
       (some  #'(lambda (allowed-element-class)
		  (find-element-class-matching-tag
		   allowed-element-class tag-name))

	      (node-class-allowed-elements test-element-class)))
   (class-precedence-list parent-element-class)))
   


;(defmethod process-element ((parent-element element) tag-name attributes)
;  (find-allowed-element parent-element tag-name)
;(defgeneric process-attribute (element attr-name attr-value))

(defgeneric find-slot-matching-attribute (element attr-name) )
(defmethod find-slot-matching-attribute ((element element) attr-name)
  (let ((element-metaclass (class-of element)))
    (find-if 
     #'(lambda (slot-definition)
	 (find-if
	  #'(lambda (slot-attribute-descriptor)
	      (descriptor-matches? slot-attribute-descriptor
				   attr-name))
	  (element-slot-attributes slot-definition)))
     (class-slots element-metaclass))))

(define-condition encountered-unmatched-attribute ()
  ())
(defgeneric assign-attribute (element name value) )
(defmethod assign-attribute ((element element) name value)
  (let ((matching-slot-definition (find-slot-matching-attribute element name)))
    (if matching-slot-definition
	(setf (slot-value-using-class (class-of element)
				      element
				      matching-slot-definition)
	      value)
	(restart-case (error (make-condition 'encountered-unmatched-attribute))
	  (continue ())))))

(defgeneric assign-attributes (element attributes) )
(defmethod assign-attributes ((element element) attributes)
  (format t "Should be assigning some attributes right now...~%")
  (mapcar #'(lambda (attrib-entry)
	      (assign-attribute element 
				(string (car attrib-entry))
				(cdr attrib-entry)))
	  attributes))

(define-condition encountered-unmatched-subelement ()
  ())

(defgeneric assign-subelement (parent-element subelement) )
(defmethod assign-subelement ((parent-element element) subelement)
  (let ((matching-slot-definition (find-slot-matching-subelement
				   parent-element subelement)))
    (if matching-slot-definition
	(setf (slot-value-using-class (class-of element)
				      element
				      matching-slot-definition)
	      value)
	(restart-case (error (make-condition 'encountered-unmatched-subelement))
	  (continue ())))))
  (format t "Should be assigning a sub element right now...~%"))
(defmethod assign-subelement ((parent-element element-class) subelement)
  (format t "Nothing happens when attempting to assign subelements to an
element class.~%"))

(define-condition encountered-unknown-element () ())

(defclass parse-seed ()
  ((element-stack :initarg :element-stack :initform nil
		  :accessor seed-element-stack)))

(defun active-handle-new-element (name attributes element-stack allowed-root-element-classes)
  "Called when an element is encountered and we are in the process
of churning out objects."
  (let ((new-element-class
	 (if (first element-stack)
	     (find-allowed-element (first element-stack)
				   (string name))
	     (some #'(lambda (allowed-element-class)
		       (find-element-class-matching-tag allowed-element-class name))
		   allowed-root-element-classes))))
    (if (null new-element-class)
	(progn
	  (restart-case (error (make-condition 'encountered-unknown-element))
	    (continue ()))
	  element-stack)
	(let ((new-element (make-instance new-element-class)))
	  (assign-attributes new-element attributes)
	  (when (not (null (first element-stack)))
	    (assign-subelement (first element-stack) new-element))
	  (append (list new-element) element-stack)))))
  

(defun active-handle-finish-element (name attributes parent-seed seed)
  "Called when the end of an element is encountered and we are in the process
of churning out objects."
  (format t "Parent seed: ~A~%Seed: ~A~%" parent-seed seed)
  (rest seed))

(defun active-handle-text (string seed)
  "Called when text is encountered and we are in the process of churning out objects."
  (format t "Seed: ~A~%" seed)
  seed)

(defun active-parse-stream (stream doc-class)
  "This is where we interact with s-xml."
  (s-xml:start-parse-xml
   stream
   (make-instance 's-xml:xml-parser-state
		  :new-element-hook 'active-handle-new-element
		  :finish-element-hook 'active-handle-finish-element
		  :text-hook 'active-handle-text
		  :seed (list doc-class))))

(defun parse-xml-stream (stream acceptable-root-classes)
  "This is where we interact with s-xml."
  (s-xml:start-parse-xml
   stream
   (make-instance 's-xml:xml-parser-state
		  :new-element-hook #'(lambda (name attribs seed)
					(active-handle-new-element
					 name attribs seed acceptable-root-classes))
		  :finish-element-hook 'active-handle-finish-element
		  :text-hook 'active-handle-text
		  :seed (list) )))
