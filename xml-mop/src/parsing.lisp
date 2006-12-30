(in-package :org.iodb.xml-mop)

(defgeneric process-element (parent-element tag-name attributes)
  (:documentation "Adds the child element with the given tag name and
 attribs to the parent elem."))

(defgeneric find-allowed-element (parent-element tag-name)
  (:documentation "Finds the element class underneath parent-element that
corresponds to the tag string."))

(defun descriptor-matches? (descriptor test-string)
  (format t "Checking to see if ~A matches ~A" 	   (descriptor-matcher descriptor) test-string)
  (funcall (if (descriptor-case-sensitive descriptor)
	       #'equal
	       #'equalp)
	   (descriptor-matcher descriptor)
	   test-string))

(defmethod find-allowed-element ((parent-element element) tag-name)
  (find-allowed-element (class-of parent-element) tag-name))

(defmethod find-allowed-element ((parent-element xml-treenode-class) tag-name)
  (find-if #'(lambda (allowed-element-class)
	       (labels ((find-matching-descriptor (element-class)
			  (format t "Searching class ~A for a matching tag descriptor~%" element-class) 
			  (or (find-if #'(lambda (descriptor)
					   (descriptor-matches? descriptor tag-name))
				       (element-class-tag-descriptors element-class))
			      (find-if #'find-matching-descriptor
				       (class-direct-subclasses element-class)))))
		 (find-matching-descriptor allowed-element-class)))
	   (node-class-allowed-elements parent-element)))

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
	      (assign-attribute element (string (car attrib-entry))
				(cdr attrib-entry)))
	  attributes))

(defgeneric assign-subelement (parent-element subelement) )
(defmethod assign-subelement ((parent-element element) subelement)
  (format t "Should be assigning a sub element right now...~%"))
(defmethod assign-subelement ((parent-element element-class) subelement)
  (format t "Nothing happens when attempting to assign subelements to an
element class.~%"))

(define-condition encountered-unknown-element () ())

(defclass parse-seed ()
  ((element-stack :initarg :element-stack :initform nil
		  :accessor seed-element-stack)))

(defun active-handle-new-element (name attributes element-stack)
  "Called when an element is encountered and we are in the process
of churning out objects."
  (let ((new-element-class (find-allowed-element (first element-stack)
						 (string name))))
    (if (null new-element-class)
	(progn
	  (restart-case (error (make-condition 'encountered-unknown-element))
	    (continue ()))
	  element-stack)
	(let ((new-element (make-instance new-element-class)))
	  (assign-attributes new-element attributes)
	  (assign-subelement (first element-stack) new-element)
	  (append (list new-element) element-stack)))))
  

(defun active-handle-finish-element (name attributes parent-seed seed)
  "Called when the end of an element is encountered and we are in the process
of churning out objects."
  (format t "Parent seed: ~A~%Seed: ~A~%" parent-seed seed)
  seed)

(defun active-handle-text (string seed)
  "Called when text is encountered and we are in the process of churning out objects."
  (format t "Seed: ~A~%" seed)
  seed)

(defun active-parse-stream (stream doc-class)
  "This is where we interact with s-xml."
  (s-xml:start-parse-xml
   stream
   (make-instance 's-xml:xml-parser-state
		  :new-element-hook 'active-handle-new-pelement
		  :finish-element-hook 'active-handle-finish-element
		  :text-hook 'active-handle-text
		  :seed (list doc-class))))
