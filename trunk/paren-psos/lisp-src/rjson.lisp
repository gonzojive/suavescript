(in-package org.iodb.paren-psos)

;;; red's javascript object notation

(defparameter *rjson-session* nil)

(defclass rjson-history-entry ()
  ((parenscript-form :accessor parenscript-form :initform nil :initarg :parenscript-form
		     :initarg :form)
   (anchor-index :accessor anchor-index :initform :index :initarg :anchor-index :initarg :anchor)
   (refcount :accessor refcount :initarg :refcount :initform 1)))
   
(defclass rjson-serialization-session ()
  ((history :accessor serialization-history :initform (make-hash-table :test #'eq)
	    :initarg :history :documentation "maps an object to a history entry")
;   (counter :accessor serialization-counter :initform -1)
;   (counter-pass2 :accessor serialization-second-pass-counter :initform -1)
   (history-vector :accessor serialization-history-vector :initform (make-array 10 :fill-pointer 0 :adjustable t)
		   :documentation "A vector of parenscript forms in the history."))
  (:documentation "Contains information relevant for serializing objects into json with crossreferencing."))

(defgeneric serialization-declare-xref (session anchor-index parenscript-form))
(defmethod serialization-declare-xref ((session rjson-serialization-session) anchor-index parenscript-form)
  (let ((history-entry (serialization-history-entry session anchor-index)))
    (when (null history-entry)
      (error "Declaring ref on null history object."))
    (setf (parenscript-form history-entry)
	  parenscript-form)))


(defgeneric serialization-xref (session anchor-index))
(defmethod serialization-xref ((session rjson-serialization-session) anchor-index)
  (let ((history-entry (serialization-history-entry session anchor-index)))
    (values (and history-entry (parenscript-form history-entry))
	    history-entry)))

(defgeneric serialization-history-entry (session anchor-index))
(defmethod serialization-history-entry ((session rjson-serialization-session) anchor-index)
  (let* ((history-vector (serialization-history-vector session))
	 (history-entry (and (< anchor-index (length history-vector))
			     (aref history-vector anchor-index))))
    history-entry))


;(defgeneric serialization-get-next-count (session &key pass))
;(defmethod serialization-get-next-count ((session rjson-serialization-session) &key (pass 1))
;  (case pass
;    (1 (incf (serialization-counter session)))
;    (2 (incf (serialization-second-pass-counter session)))))

(defgeneric register-xref-object (session object))
(defmethod register-xref-object ((session rjson-serialization-session) object)
  (let* ((current-val (gethash object (serialization-history session)))
	 (object-anchor-index current-val)
	 (history-vector (serialization-history-vector session))
	 (history-object (if object-anchor-index
			     (progn
;			       (format t "found history object for ~A~%" object)
			       (serialization-history-entry session object-anchor-index))
			     (progn
;			       (format t "making history object for ~A~%" object)
			       (make-instance 'rjson-history-entry :refcount 0
					      :anchor-index (length history-vector))))))
;    (format t "Attempting to register @ ~A with ~A~%" (or current-val (length history-vector)) object)
;				  (< object-anchor-index (length history-vector)))
;			     (aref history-vector object-anchor-index)

    (when (null current-val)
      (vector-push-extend history-object
			  (serialization-history-vector session))
      (setf (gethash object (serialization-history session))
	    (anchor-index history-object)))

    (when (some #'null (serialization-history-vector *rjson-session*))
      (error "Should never have null entries in history vector2."))

    (when (null history-object)
      (error "NULL history object, current val ~A : ~A -> ~A~%~A"
	     current-val object-anchor-index history-object history-vector))


    (incf (refcount history-object))
    (values
     (anchor-index history-object)
     (not (null current-val)))))

(defgeneric represent-rjson (object &optional seed)
  (:documentation "Returns a parenscript form representing the given object.
Encoding an object into rjson is a two-pass process. Methods defined for custom
objects will typically make subsequent calls to represent."))

(defgeneric is-xrefable? (object &optional seed)
  (:documentation "Returns whether the given object should be considered for
cross-referencing.  Generally this will return true unless it's a simple datatype."))

(defun represent (object &optional seed)
  "This is the function users call to generate an appropriate parenscript form with embeddded
cross-references.  This should be called instead of represent-rjson on objects in custom
represent-rjson methods."
  (if (is-xrefable? object seed)
      (multiple-value-bind (object-anchor object-in-history?)
	  (register-xref-object *rjson-session* object)
	(when (not object-in-history?)
	  (serialization-declare-xref *rjson-session* object-anchor
				      (represent-rjson object seed)))
	(if object-in-history?
	    `(rjson-xref ,object-anchor)
	    `(rjson-xdecl ,object-anchor ,(represent-rjson object seed)))
	(represent-rjson object seed))))

(js:defjsmacro rjson-xdecl (index object)
  "This is an internal parenscript macro used to serialize the generated rjson-
parenscript forms.  It replaces an (rejson-xdecl index paren-form) with either
an (xdecl index paren-form) or paren-form depending on whether a cross-ref is
necessary."
  (multiple-value-bind (parenscript-form history-object)
      (serialization-xref *rjson-session* index)
    (when (some #'null (serialization-history-vector *rjson-session*))
      (error "Should never have null entries in history vector"))
    (when (null history-object)
      (error "INVALID HISTOROY OBJECT HOW THIS HAPPEN? ~A~%~A" index
	      (serialization-history-vector *rjson-session*)))
    (format t "parenscript form ~A~%"	  parenscript-form)
    (let ((refcount (refcount history-object)))
;      (format t "REFCOUNT: ~A~%" refcount)
      (if (= 1 refcount)
	  parenscript-form
	  `(xdecl
	    ,index
	    ,(serialization-declare-xref *rjson-session* index object))))))
  
(js:defjsmacro rjson-xref (index)
  "This is an internal parenscript macro used to serialize the generated rjson-
parenscript forms.  It replaces an (rejson-xdecl index) with either
an (xref index) or raw paren-form depending on whether a cross-ref is necessary."
  `(xref ,index))

;;; USER-LEVEL
(defun write-rjson (object)
  "Writes an object out as a javascript string.  Calls the customizable
represent-rjson function on object to determine its parenscript form,
then serializes the output using the parenscript compiler."
  (let* ((*rjson-session* (make-instance 'rjson-serialization-session))
	 (result-paren-list (represent object)))
;    (with-open-file (fout "/tmp/balderdash.lisp" :direction :output :if-exists :supersede)
;      (print result-paren-list fout))
;    (js:js-to-string result-paren-list)))
))

(defmethod is-xrefable? (object &optional seed)
  (declare (ignore object) (ignore seed))
  t)
(defmethod is-xrefable? ((object string) &optional seed)
  (declare (ignore object) (ignore seed))
  nil)
(defmethod is-xrefable? ((object number) &optional seed)
  (declare (ignore object) (ignore seed))
  nil)

(defmethod represent-rjson (object &optional seed)
  object)

(defmethod represent-rjson ((object hash-table) &optional seed)
  (let ((create-args (list)))
    (maphash #'(lambda (key value)
		 (push key create-args)
		 (push (represent value seed) create-args))
	     object)
    (apply #'list 'create (nreverse create-args))))

(defmethod represent-rjson ((object sequence) &optional seed)
  `(array ,@(handler-case (map 'list #'(lambda (obj)
					 (represent obj seed))
			       object)
			  (error () "stupid"))))
			  
			  

(defmethod represent-rjson ((object string) &optional seed)
  object)
