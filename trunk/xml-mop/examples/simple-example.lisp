(in-package :xml-mop)

(defclass strange-price-element ()
  ((currency-code :accessor price-strange-code
		  :attribute "StrangePrice"
		  :initarg nil))
  (:metaclass element-class))

(defclass price-element (strange-price-element)
  ((currency-code :accessor price-currency-code
		  :initarg :currency-code
;		  :attribute ("CurrencyCode" :primary t :case-sensitive t)
		  :attribute "currency-code"
;		  :attribute "currencyCode"
		  ))
  (:metaclass element-class)
  (:tags ("hello" :primary t :case-sensitive t)
	 ("Price" :sweet t )))

;(defclass item-element ()
;  ((asin :accessor item-asin :initform "" :initarg :asin)
;   (detail-page-url :accessor item-detail-page-url
;		    :initform nil
;		    :subelement "DetailPageURL"))
;  (:metaclass element-class)


;(defelement price-element ()
;  (amount :accessor price-amount :initform 0)
;  (currency-code :accessor price-currency-code
;		 :initform ""
;		 :attr-string "CurrencyCode"
;		 
;		 :attr-string ("Code" t) ; case-sensitive attribute
;		 :attribute-function ()) ; there should be a way to specify an arbitrary function
;  
;)