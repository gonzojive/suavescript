(in-package :org.iodb.amazon.ecs)

(defconstant +amazon-en-base-url+
  "http://webservices.amazon.com/onca/xml?Service=AWSECommerceService")

(defun join-string-list (string-list &optional (delimiter ""))
  "Concatenates a list of strings and puts spaces between the elements."
  (let ((control-str (concatenate 'string "~{~A~^" delimiter "~}")))
    (format nil control-str string-list)))

(defun hyphenated->camelized (hyphen-word)
  "Returns a camelized version of the hyphenated string of words."
  (let ((words (cl-ppcre:split "-" hyphen-word)))
    (join-string-list
     (mapcar #'string-capitalize words))))

(defun amazon-query-component (name value)
  (format nil "~A=~A"
	  (trivial-http:escape-url-query (hyphenated->camelized (string name)))
	  (trivial-http:escape-url-query value)))

(defmacro urlize-keys (url-key-forms)
  `(list 
    ,@(mapcar
       #'(lambda (url-key-form)
	   (let* ((true-url-key (if (listp url-key-form) (second url-key-form) url-key-form))
		  (url-key-sym (if (listp url-key-form) (first url-key-form) url-key-form))
		  (url-key-value (intern (string url-key-sym))))
	     `(if ,url-key-value
	       ,(list 'amazon-query-component true-url-key url-key-value)
	       nil)))
       url-key-forms)))

(defun generate-ecs-url (&key aws-key associate-id operation
			 response-group item-id keywords search-index
			 merchant-id condition delivery-method id-type
			 ispu-postal-code version)
  "Generates an ECS url with the supplied parameters."
  (join-string-list
   (list +amazon-en-base-url+
	 (join-string-list
	  (remove-if #'null 
		     (urlize-keys
		      (:search-index :operation :keywords :item-id :version
				     :response-group :merchant-id :id-type :condition :delivery-method
				     (:ispu-postal-code :i-s-p-u-postal-code)
				     (:aws-key :a-w-s-access-key-id)
				     (:associate-id :associate-tag))))
	  "&")) "&"))

(defun generate-ecs-url-old (&key aws-key associate-id operation keywords search-index)
  "Generates an amazon URL given a bunch of arguments."
    (format nil "&AWSAccessKeyId=~A&Operation=~A&AssociateTag=~A&Keywords=~A&SearchIndex=~A"
	    aws-key operation associate-id keywords search-index))


(defun read-response-xml (response-xml)
  (xml:read response-xml))
