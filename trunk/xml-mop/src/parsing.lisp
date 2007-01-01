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
(defmethod find-slot-matching-subelement ((non-parent-element-class t) tag-name)
  (declare (ignore tag-name) (ignore non-parent-element-class))
  nil)
(defmethod find-slot-matching-subelement ((parent-element-class element-class) tag-name)
  (let ((match
	 (some
	  #'(lambda (slot-definition)
	      (some #'(lambda (slot-subelement-descriptor)
			(let ((matching-class
			       (or (find-element-class-matching-tag
				    (find-class (descriptor-element-type slot-subelement-descriptor))
				    tag-name)
				   (some #'(lambda (tag-alias-descriptor)
					     (when (descriptor-matches? tag-alias-descriptor tag-name)
					       (find-class (descriptor-element-type slot-subelement-descriptor))))
					 (descriptor-tag-aliases slot-subelement-descriptor)))))
			  (when matching-class
			    (list slot-definition matching-class))))
		    (element-slot-subelements slot-definition)))
	  (class-slots parent-element-class))))
    (if match
	(values-list match))))

;  (values-list
;   (some
;    #'(lambda (slot-definition)
;	(some #'(lambda (slot-subelement-descriptor)
;		  (when (descriptor-matches? slot-subelement-descriptor tag-name)
;		    (list slot-definition 
;			  (find-class (descriptor-element-type slot-subelement-descriptor)))))
;	      (element-slot-subelements slot-definition)))
;    (class-slots parent-element-class))))

(defmethod find-subelement-matching-tag ((parent-element-class element-class) tag-name)
  (let ((match
    ; look through all the superclasses of our element-class to
    ; see if we find a match anywhere in the inheritance chain
	 (some #'(lambda (test-element-class)
		   (let ((matching-slot-and-element-class
			  (multiple-value-list ; should return (slot-def element-class)
			   (find-slot-matching-subelement test-element-class tag-name))))
		     (when (first matching-slot-and-element-class)
		       matching-slot-and-element-class)))
	       (class-precedence-list parent-element-class))))
    (when match
      (values-list (reverse match)))))
  
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
(defgeneric assign-attribute (element name attr-value) )
(defmethod assign-attribute ((element element) name attr-value)
  (let ((matching-slot-definition (find-slot-matching-attribute element name)))
    (if matching-slot-definition
	(setf (slot-value-using-class (class-of element)
				      element
				      matching-slot-definition)
	      attr-value)
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
	  (continue ()))))
  (format t "Should be assigning a sub element right now...~%"))

(defmethod assign-subelement ((parent-element element-class) subelement)
  (format t "Nothing happens when attempting to assign subelements to an
element class.~%"))

(define-condition encountered-unknown-element () ())

;; the seed used is a list of the form
;; (allowed-root-element-classes root-elements element-stack-element*)
(defun active-handle-new-element (name attributes seed)
  "Called when an element is encountered and we are in the process
of churning out objects."
  (multiple-value-bind  (element-stack allowed-root-element-classes root-elements)
      (destructure-seed seed)
    (multiple-value-bind (new-element-class parent-slot)
	(if (first element-stack) ; if there are any elements on the stack
	    (find-subelement-matching-tag (class-of (first element-stack))
					  (string name))
	    (values
	     (some #'(lambda (allowed-element-class)
		       (find-element-class-matching-tag allowed-element-class name))
		   allowed-root-element-classes)
	     nil))
      (if (null new-element-class)
	  (progn
	    (restart-case (error "encountered unknown element ~A" name) ;(make-condition 'encountered-unknown-element))
	      (continue ()))
	    seed)
	  (let ((new-element (make-instance new-element-class)))
	    (assign-attributes new-element attributes)
	    (when (not (null parent-slot))
	      (setf (slot-value-using-class (class-of (first element-stack))
					    (first element-stack)
					    parent-slot)
		    new-element))
	    (generate-seed (append (list new-element) element-stack)
			   allowed-root-element-classes
			   (if (null element-stack)
			       (append root-elements (list new-element))
			       root-elements)))))))
  

(defun destructure-seed (seed)
;  (format t "Destructuring seed ~A~% w/ root elements ~A~%" seed (second seed))
  (values
   (rest (rest seed)) ;element-stack
   (first seed) ;allowed-root-element-classesn
   (second seed))) ;root-elements

(defun generate-seed (element-stack allowed-root-element-classes root-elements)
  (append (list allowed-root-element-classes)
	  (list root-elements)
	  element-stack))


(defgeneric finalize-after-parse (element))
(defmethod finalize-after-parse ((element element))
  nil)
  

(defun active-handle-finish-element (name attributes parent-seed seed)
  "Called when the end of an element is encountered and we are in the process
of churning out objects."
  (declare (ignore attributes) (ignore name))
;  (format t "Parent seed: ~A~%Seed: ~A~%" parent-seed seed)
  (multiple-value-bind  (element-stack allowed-classes root-elements)
      (destructure-seed seed)
    (finalize-after-parse (first element-stack))
    (generate-seed (rest element-stack) allowed-classes root-elements)))

(defun active-handle-text (string seed)
  "Called when text is encountered and we are in the process of churning out objects."
;  (format t "Seed: ~A~%" seed)
  (let ((element-stack (rest (rest seed))))
    (if (first element-stack)
	(setf (element-text (first element-stack))
	      (concatenate 'string (element-text (first element-stack)) string))
	(error "Somehow got text when there is no element stack.")))
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
  (multiple-value-bind (element-stack allowed-root-classes root-elements)
      (let ((terminal-seed
	     (s-xml:start-parse-xml
	      stream
	      (make-instance 's-xml:xml-parser-state
			     :new-element-hook 'active-handle-new-element
			     :finish-element-hook 'active-handle-finish-element
			     :text-hook 'active-handle-text
			     :seed (generate-seed nil acceptable-root-classes nil)))))
	(destructure-seed terminal-seed))
    (declare (ignore element-stack) (ignore allowed-root-classes))
    root-elements))
    