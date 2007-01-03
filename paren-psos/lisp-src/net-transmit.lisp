(in-package org.iodb.paren-psos)

;;; This File Includes Utilities for sending CLOS instances across the net as
;;; cross-referencing JSON

;; to be specific, CLOS expresses more in first-order objects than does PSOS
;; generic functions, methods, and method combinations will probably be supported
;; as first-order objects in the PSOS runtime in the future because they are already
;; expressed in the runtime

;; cross-referencing JSON works similarly to normal JSON except that
;; duplicate objects (as given by the results of an eq comparison)
;; are only stored once, and then in the future backreferenced

(defparameter *json-session* nil)

(defclass rjson-history-entry ()
  ((parenscript-form :accessor parenscript-form :initform nil :initarg :parenscript-form
		     :initarg :form)
   (anchor-index :accessor anchor-index :initform :index :initarg :anchor-index :initarg :anchor)
   (refcount :accessor refcount :initarg :refcount :initform 1)))
   
(defclass rjson-serialization-session ()
  ((history :accessor serialization-history :initform (make-hash-table :test #'eq)
	    :initarg :history :documentation "maps an object to a history entry")
   (counter :accessor serialization-counter :initform -1)
   (counter-pass2 :accessor serialization-second-pass-counter :initform -1)
   (history-vector :accessor serialization-history-vector :initform (make-array 10 :fill-pointer 0 :adjustable t)
		   :documentation "A vector of parenscript forms in the history."))
  (:documentation "Contains information relevant for serializing objects into json with crossreferencing."))

(defgeneric serialization-declare-xref (session anchor-index parenscript-form))
(defmethod serialization-declare-xref ((session rjson-serialization-session) anchor-index parenscript-form)
  (let ((history-object
	 (aref (serialization-history-vector session) anchor-index)))
    (setf (parenscript-form history-object) parenscript-form)))
	
(defgeneric serialization-xref (session anchor-index))
(defmethod serialization-xref ((session rjson-serialization-session) anchor-index)
  (let ((history-entry
	 (aref (serialization-history-vector session) anchor-index)))
    (values (parenscript-form history-entry)
	    history-entry)))

(defgeneric serialization-get-next-count (session &key pass))
(defmethod serialization-get-next-count ((session rjson-serialization-session) &key (pass 1))
  (case pass
    (1 (incf (serialization-counter session)))
    (2 (incf (serialization-second-pass-counter session)))))

(defgeneric register-xref-object (session object))
(defmethod register-xref-object ((session rjson-serialization-session) object)
  (let* ((current-val (gethash object (serialization-history session)))
	 (object-anchor (or (car current-val)
			    (serialization-get-next-count session)))
	 (object-refcount (or (cdr current-val) 0))
    
    (format t "Registering new object ~A~% at index ~A~%" object object-anchor)
    (setf (gethash object (serialization-history session))
	  (cons object-anchor (+ 1 object-refcount)))
    (values object-anchor
	    (not (null current-val))))))

(defmethod register-xref-object ((session rjson-serialization-session) object)
  (let* ((current-val (gethash object (serialization-history session)))
	 (object-anchor-index current-val)
	 (history-object (if object-anchor-index
			     (aref (serialization-history-vector session) object-anchor-index)
			     (make-instance 'rjson-history-entry :refcount 0
					    :anchor-index (length (serialization-history-vector session))))))

    (when (null current-val)
      (vector-push history-object
		   (serialization-history-vector session))
      (setf (gethash object (serialization-history session))
	    (anchor-index history-object))
      (incf (refcount history-object)))
    (values
     (anchor-index history-object)
     (not (null current-val)))))

(defgeneric serialization-representation (session object)
  (:documentation "The (parenscript) representation of the given object"))

(defmethod serialization-representation ((session rjson-serialization-session) object)
  "This was refACtored.")

(defgeneric represent-rjson (object &optional seed)
  (:documentation "Returns a parenscript form representing the given object.
Encoding an object into rjson is a two-pass process. method defined for custom
objects will typically make subsequent calls to represent.  repre
as 
"))

(defparameter *rjson-session* nil)

(defun represent (object &optional seed)
  "This is the function users call to generate an appropriate parenscript form with embeddded
cross-references.  This should be called instead of represent-rjson on objects in custom
represent-rjson methods."
  (multiple-value-bind (object-anchor object-in-history?)
      (register-xref-object *rjson-session* object)
    (if object-in-history?
	(progn
	  (format t "Exists! Did not recompute new rjson representation for object: ~A~%" object)
	  `(rjson-xref ,object-anchor))
	(progn
	  (format t "Added new object to history: ~A~%" object)
	  `(rjson-xdecl ,object-anchor ,(represent-rjson object seed))))))

(js:defjsmacro rjson-xdecl (index object)
  "This is an internal parenscript macro used to serialize the generated rjson-
parenscript forms.  It replaces an (rejson-xdecl index paren-form) with either
an (xdecl index paren-form) or paren-form depending on whether a cross-ref is
necessary."
  `(xdecl
    ,index
    ,(serialization-declare-xref *rjson-session* index object)))

(js:defjsmacro rjson-xref (index)
  "This is an internal parenscript macro used to serialize the generated rjson-
parenscript forms.  It replaces an (rejson-xdecl index) with either
an (xref index) or raw paren-form depending on whether a cross-ref is necessary."
  `(xref ,index))


(defun write-rjson (object)
  (let ((*rjson-session* (make-instance 'rjson-serialization-session)))
    (js:js-to-string
     (represent object))))

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
  `(array ,@(mapcar #'(lambda (obj)
			(represent obj seed))
		    object)))

(defmethod represent-rjson ((object string) &optional seed)
  object)