(in-package :xml-mop)

(defclass aws-response-document ()
  ()
  (:metaclass element-class)
  (:allowed-elements abstract-base-response))

(defclass abstract-base-response ()
  ()
  (:metaclass element-class)
  (:allowed-elements price-element))

(defclass item-search-response (abstract-base-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemSearchResponse" :primary t)))

(defclass price-element ()
  ((currency-code :accessor price-currency-code
		  :initarg :currency-code
		  :attribute ("CurrencyCode" :primary t :case-sensitive t)
		  :attribute ("Bogus")
;		  :attribute "currency-code"
;		  :attribute "currencyCode"
		  ))
  (:metaclass element-class)
  (:tags ("Price" :primary t :case-sensitive nil)
	 ("PriceElement" :case-sensitive nil)))

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