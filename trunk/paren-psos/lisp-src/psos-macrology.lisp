(in-package org.iodb.paren-psos)

(defparameter *client-debug* t)
(defun client-debug () *client-debug*)
(js:defjsmacro alert (message)
  `(log ,message :warn))
(js:defjsmacro defun2 (formal-name arguments &rest body)
  `(js:defvar ,formal-name (lambda ,arguments ,@body)))

(js:defjsmacro effective-throw (error-obj)
  `(progn
    (log ,error-obj :error)
    (console.trace)
    (throw ,error-obj)))

(js:defjsmacro log (message &optional level (scalar-level 100))
  (if (and (client-debug)
;	   nil)
	   t)
;	   (or (> scalar-level 10000) (eql :error level)))
;	   (or (> scalar-level 101) (equal :error level) (equal :warn level)))
      `(if console
	,(case level
	  (:error `(console.error ,message))
	  (:warn `(console.warn ,message))
	  (:warning `(console.warn ,message))
	  ((:info nil) `(console.info ,message)))
	,(if (eql level :error)
	     `(window.alert ,message)))
      (values)))
	

(js:defjsmacro debug-time-start (str)
  (if *client-debug*
      `(if console (console.time ,str))))

(js:defjsmacro debug-time-end (str)
  (if *client-debug*
      `(if console (console.time-end ,str))))

(js:defjsmacro with-debug-timer (str &rest body)
  `(progn
    (debug-time-start ,str)
    ,@body
    (debug-time-end ,str)))

(js:defjsmacro defclass (class-name &optional superclasses direct-slots options)
  
  `(progn
    (setf ,class-name (create-class 
		       ,(js:js-to-string class-name)
		       ,(if (null superclasses)
			    nil
			    `(array ,@superclasses))))))

(js:defjsmacro defaultf (place default-val)
  `(setf ,place (or ,place ,default-val)))

(js:defjsmacro defgeneric (formal-name)
  `(defvar ,formal-name (ensure-generic ,formal-name ,(js:js-to-string formal-name))))

(defun parse-defjsmethod-args (args)
  (let ((fn-name (first args))
	(qualifiers nil)
	(lambda-list nil)
	(specializers nil)
	(body nil))
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

(js:defjsmacro dolist2 (i-array &rest body)
  (js:with-unique-js-names (arrvar idx)
    (let ((var (first i-array))
	  (array (second i-array))
	  (direction (or (third i-array) :forward)))
      `(let ((,arrvar ,array))
	(do ((,idx
	      ,@(if (eql :forward direction)
		    `(0 (1+ ,idx))
		    `((1- (slot-value ,arrvar 'length)) (1- ,idx)))))
	    (,(if (eql :forward direction)
		  `(>= ,idx (slot-value ,arrvar 'length))
		  `(< ,idx 0)))
	  (let ((,var (aref ,arrvar ,idx)))
	    ,@body))))))

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
