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
