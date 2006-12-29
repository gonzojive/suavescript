(in-package :org.iodb.xml-mop)

(defgeneric add-node-description (parser node-descriptor target-class-or-lambda)
  (:documentation "Adds a handler to a parser.  This handler should be
either a function or an element class.  If it is a function, when called
with a tag string and attributes it should return a value."))

(defmethod add-node-description ((parser standard-parser) (descriptor named-node-descriptor) target-class-or-lambda)
  (if (stringp (descriptor-matcher descriptor))
      (setf (gethash (descriptor-matcher descriptor)
		     (if (descriptor-case-sensitive descriptor)
			 (parser-case-sensitive-table parser)
			 (parser-case-insensitive-table parser)))
	    target-class-or-lambda)
      (push (cons (descriptor-matcher descriptor)
		  target-class-or-lambda)
	    (slot-value parser 'lambda-table))))

(defgeneric find-element-handler (parser tag-name)
  (:documentation "Looks up an element in a parser."))

(defmethod find-element-handler ((parser standard-parser) tag-name)
  (let ((match
	 (or (gethash tag-name (parser-case-sensitive-table parser))
	     (gethash tag-name (parser-case-insensitive-table parser))
	     (find-if #'(lambda (entry)
			  (funcall (car entry) tag-name))
		      (parser-lambda-table parser)))))
    match))

;(defgeneric element-value (parser tag-name attributes)
;  (:documentation "Creates an element given an active parser, tag name and attributes."))

(defmethod make-element-instance ((parser standard-parser) tag-name attributes)
  (let ((handler (find-element-handler parser tag-name)))
    (if (subtypep handler (find-class 'element))
	(make-instance element 

(defun active-handle-new-element (name attributes seed)
  "Called when an element is encountered and we are in the process
of churning out objects."
  (print (stringp name))
  seed)

(defun active-handle-finish-element (name attributes parent-seed seed)
  "Called when the end of an element is encountered and we are in the process
of churning out objects."
  (format t "Parent seed: ~A~%Seed: ~A~%" parent-seed seed)
  seed)

(defun active-handle-text (string seed)
  "Called when text is encountered and we are in the process of churning out objects."
  (format t "Seed: ~A~%" seed)
  seed)

(defun active-parse-stream (stream)
  "This is where we interact with s-xml."
  (s-xml:start-parse-xml
   stream
   (make-instance 's-xml:xml-parser-state
		  :new-element-hook 'active-handle-new-element
		  :finish-element-hook 'active-handle-finish-element
		  :text-hook 'active-handle-text)))
