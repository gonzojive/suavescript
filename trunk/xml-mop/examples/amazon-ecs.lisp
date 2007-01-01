(defpackage :my-amazon-ecs
  (:nicknames :ecs2)
  (:use :cl))

(in-package :my-amazon-ecs)

(defmacro defelement (class-name direct-superclasses slots &rest options)
  `(defclass ,class-name ,direct-superclasses 
    ,slots
    ,options
    (:metaclass element-class)))

(import 'xml-mop::element-class)
;;; Class definitions for AWS responses


(defclass abstract-root-response ()
  ((xmlns :accessor response-xmlns :initform "" :initarg :xmlns
	  :attribute ("xmlns"))
   (operation-request :accessor response-operation-request :initform nil :initarg :operation-request
		      :subelement (:element-type operation-request))
   (items :accessor response-items :initform nil :initarg :items
	  :subelement (:element-type items)))
  (:metaclass element-class)
  (:documentation "root of most all operations response"))


(defclass amount () () (:metaclass element-class) (:tags ("Amount")))
(defclass currency-code () () (:metaclass element-class) (:tags ("CurrencyCode")))
(defclass formatted-price () () (:metaclass element-class) (:tags ("FormattedPrice")))

(defclass price ()
  ((amount :accessor price-amount :initform 0 :initarg :amount
	   :subelement (:element-type amount))
   (currency-code :accessor price-currency-code :initform 0 :initarg :currency-code
		 :subelement (:element-type currency-code))
   (formatted-price :accessor price-formatted :initform 0 :initarg :formatted-price
		   :subelement (:element-type formatted-price)))
  (:metaclass element-class)
  (:tags ("Price"))
  (:documentation "Parent of all elements that contain price information"))
(defclass lowest-used-price (price) () (:metaclass element-class) (:tags ("LowestUsedPrice")))
(defclass lowest-collectible-price (price) () (:metaclass element-class) (:tags ("LowestCollectiblePrice")))
(defclass lowest-new-price (price) () (:metaclass element-class) (:tags ("LowestNewPrice")))
(defclass lowest-refurbished-price (price) () (:metaclass element-class) (:tags ("LowestRefurbishedPrice")))


(defclass item-search-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemSearchResponse"))
  (:documentation "root of item search response"))

(defclass item-lookup-response (abstract-root-response)
  ()
  (:metaclass element-class)
  (:tags ("ItemLookupResponse"))
  (:documentation "root of item lookup responses"))

(defclass operation-request-id () () (:metaclass element-class) (:tags ("RequestId")))
(defclass operation-request ()
  ((http-headers :accessor operation-http-headers :initform () :initarg :http-headers
		 :subelement (:element-type http-headers))
   (request-id :accessor operation-requestid :initform "" :initarg requestid
	       :subelement (:element-type operation-request-id))
   (arguments :accessor operation-arguments :initform () :initarg arguments
	      :subelement (:element-type operation-arguments))
   (request-processing-time :accessor request-processing-time :initform () :initarg :request-processing-time))
  (:metaclass element-class)
  (:tags ("OperationRequest"))
  (:documentation "OperationResponse element in Amazon ECS response"))

(defclass operation-arguments ()
  ((arguments :accessor operation-arguments :initform nil
	      :subelement (:element-type operation-argument)))
  (:metaclass element-class)
  (:tags ("Arguments")))

(defclass operation-argument ()
  ((arg-name :accessor argument-name :initform "" :initarg :name :attribute ("Name"))
   (arg-value :accessor argument-name :initform "" :initarg :name :attribute ("Value")))
  (:metaclass element-class)
  (:tags ("Argument"))
  (:documentation "Arguments element in Amazon ECS response"))

(defclass operation-argument ()	      :subelement (:element-type operation-argument)))
  (:metaclass element-class)
  (:tags ("Argument")))


(defclass http-headers ()
  ((headers :accessor headers :initform () :initarg :headers
	    :subelement (:element-type http-header :multiple t)))
  (:metaclass element-class)
  (:tags ("HTTPHeaders"))
  (:documentation "HTTPHeaders element in Amazon ECS response"))  

(defclass http-header ()
  ((name :accessor name :initform "" :initarg :name :attribute ("Name"))
   (value :accessor value :initform "" :initarg :value :attribute ("Value")))
  (:metaclass element-class)
  (:tags ("Header"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass items ()
  ((request :accessor request :initform nil :initarg :request :subelement (:element-type request))
   (items :accessor items :initform () :initarg :items
	  :subelement (amazon-item :multiple t))
   (totalresults :accessor total-results :initform 0 :initarg :total-results :type integer)
   (totalpages :accessor total-pages :initform 0 :initarg :total-pages :type integer))
  (:metaclass element-class)
  (:tags ("Items"))
  (:documentation "HTTPHeader element in Amazon ECS response"))


(defclass request ()
  ((is-valid :accessor request-is-valid :initform nil :initarg :is-valid
	    :subelement (request-is-valid))
   (errors :accessor request-errors :initform nil :initarg :errors)
   (item-search-request :accessor request-item-search-request :initform ()
			:initarg :item-search-request :subelement (item-search-request))
   (item-lookup-request :accessor item-lookup-request :initform ()
			:initarg :item-lookup-request :subelement (item-lookup-request)))
  (:metaclass element-class)
  (:tags ("Request"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass request-is-valid () () (:metaclass element-class) (:tags ("IsValid")))


(defclass request-item-id () () (:metaclass element-class) (:tags ("ItemId")))
(defclass request-response-group () () (:metaclass element-class) (:tags ("ResponseGroup")))
(defclass request-merchant-id () () (:metaclass element-class) (:tags ("MerchantId")))
(defclass request-keywords () () (:metaclass element-class) (:tags ("Keywords")))
(defclass request-search-index () () (:metaclass element-class) (:tags ("SearchIndex")))

(defclass item-search-request ()
  ((keywords :accessor keywords :initform nil :initarg :keywords
	     :subelement (request-keywords))
   (merchantid :accessor merchant-id :initform nil :initarg :merchant-id
	       :subelement (request-merchant-id))
   (searchindex :accessor search-index :initform () :initarg :search-index
		:subelement (request-search-index)))
  (:metaclass element-class)
  (:tags ("ItemSearchRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))
(xml:def-element-class-name ItemSearchRequest item-search-request)

(defclass item-lookup-request ()
  ((item-id :accessor request-item-id :initform nil :initarg :item-id
	    :subelement (request-item-id))
   (response-groups :accessor response-groups :initform () :initarg :response-groups
		   :subelement (request-response-group :multiple t))
   (merchant-id :accessor merchant-id :initform () :initarg :merchant-id
		:subelement (request-merchant-id)))
  (:metaclass element-class)
  (:tags ("ItemLookupRequest"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass amazon-errors ()
  ((errors :accessor errors :initarg :errors :initform nil))
  (:metaclass element-class)
  (:tags ("Request")))

(defclass amazon-error ()
  ((code :accessor code :initarg :code :initform nil)
   (message :accessor message :initarg :message :initform ""))
  (:metaclass element-class)
  (:tags ("Request")))

;;; Amazon Item
(defgeneric title (item-like-thing)
  (:documentation "Gives the title of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric author (item-like-thing)
  (:documentation "Gives the author of an item-like thing, e.g. ItemAttributes or Item"))
(defgeneric isbn (item-like-thing)
  (:documentation "Gives the isbn of an item-like thing, e.g. ItemAttributes or Item"))

(defclass item-asin () () (:metaclass element-class) (:tags ("ASIN")))
(defclass item-detail-page-url () () (:metaclass element-class) (:tags ("DetailPageURL")))
(defclass item-sales-rank (numerical-text-element) () (:metaclass element-class) (:tags ("SalesRank")))
(defclass amazon-item ()
  ((item-attributes :accessor item-attributes :initform nil :initarg :item-attributes
		    :subelement (item-attributes))
   (asin :accessor item-asin :initform "" :initarg :asin :subelement (item-asin))
   (sails-rank :subelement (item-sales-rank))
   (large-image :subelement (large-image) :accessor item-large-image)
   (small-image :subelement (small-image) :accessor item-small-image)
   (medium-image :subelement (medium-image) :accessor item-medium-image)
   (offers :accessor offers :initform () :initarg :offers)
   (alternate-versions :accessor alternate-versions :initform nil :initarg :alternate-versions)
   (offer-summary :accessor offer-summary :initform () :initarg :offer-summary :subelement (offer-summary))
   (detail-page-url :accessor item-detail-page-url :initform "" :initarg :detail-page-url
		    :subelement (item-detail-page-url)))
  (:metaclass element-class)
  (:tags ("Item"))
  (:documentation "HTTPHeader element in Amazon ECS response"))

(defclass image-url () () (:metaclass element-class) (:tags ("URL")))
(defclass image-height () () (:metaclass element-class) (:tags ("Height")))
(defclass image-width () () (:metaclass element-class) (:tags ("Width")))
(defclass abstract-image ()
  ((url :subelement (image-url) :accessor image-url :initform "")
   (width :subelement (image-width) :accessor image-width :initform "")
   (height :subelement (image-height) :accessor image-height :initform ""))
  (:metaclass element-class))
(defclass large-image (abstract-image) () (:metaclass element-class) (:tags "LargeImage"))
(defclass medium-image (abstract-image) () (:metaclass element-class) (:tags "MediumImage"))
(defclass small-image (abstract-image) () (:metaclass element-class) (:tags "SmallImage"))

(defclass item-price-description-mixin ()
  ((lowest-new-price :accessor lowest-new-price :initform nil
		     :subelement (price-element :alias "LowestNewPrice"))
   (lowest-collectible-price :accessor lowest-collectible-price :initform nil
			     :subelement (price-element :alias "LowestCollectiblePrice"))
   (lowest-used-price :accessor lowest-used-price :initform nil
		    :subelement (price-element :alias "LowestUsedPrice"))
   (lowest-refurbished-price :accessor lowest-refurbished-price :initform nil
			     :subelement (price-element :alias "LowestRefurbishedPrice")))
  (:metaclass element-class))

(defclass offer-summary (item-price-description-mixin)
  ((totalnew :accessor summary-total-new :initform nil :initarg :total-new
	     :subelement (summary-total-new))
   (totalused :accessor lowest-used-price :initform nil :initarg :total-used
	      :subelement (summary-total-used))
   (totalcollectible :accessor lowest-used-price :initform nil :initarg :total-collectible
		     :subelement (summary-total-collectible))
   (totalrefurbished :accessor lowest-used-price :initform nil :initarg :total-refurbished
		     	      :subelement (summary-total-refurbished)))
  (:metaclass element-class)
  (:tags ("OfferSummary"))
  (:documentation "Summary of offers for a particular item"))

(defclass numerical-text-element ()
  ()
  (:metaclass element-class)
  (:documentation "An element that has text that reduces to a number"))

(defclass summary-total-new (numerical-text-element) () (:metaclass element-class) (:tags ("TotalNew")))
(defclass summary-total-used (numerical-text-element) () (:metaclass element-class)  (:tags ("TotalUsed")))
(defclass summary-total-collectible (numerical-text-element) () (:metaclass element-class)  (:tags ("TotalCollectible")))
(defclass summary-total-refurbished (numerical-text-element) () (:metaclass element-class)  (:tags ("TotalRefurbished")))

(defclass simple-text-element () () (:metaclass element-class))
(defclass numerical-measurement-element (numerical-text-element)
  ((units :attribute "Units" :accessor distance-units :initform ""))
  (:metaclass element-class))
(defclass distance-element (numerical-measurement-element) () (:metaclass element-class))
(defclass weight-element (numerical-measurement-element) () (:metaclass element-class))

(defclass price-element ()
  ((amount :accessor price-amount :initform 0 :initarg :amount
	   :subelement (numerical-text-element :alias "Amount"))
   (currency-code :accessor price-currency-code :initform 0 :initarg :currency-code
		  :subelement (simple-text-element :alias "CurrencyCode"))
   (formatted-price :accessor price-formatted :initform 0 :initarg :formatted-price
		   :subelement (simple-text-element :alias "FormattedPrice")))
  (:metaclass element-class)
  (:documentation "Parent of all elements that contain price information"))

(defclass item-attributes (item-price-description-mixin)
  ((authors :accessor authors :initform nil :initarg :authors
	    :subelement (simple-text-element :alias "Author" :multiple t))
   (features :accessor item-features :initform nil :initarg :features
	    :subelement (simple-text-element :alias "Feature" :multiple t))
   (height :accessor item-height :initform nil :initarg :height
	   :subelement (distance-element :alias "Height"))
   (length :accessor item-length :initform nil :initarg :length
	   :subelement (distance-element :alias "Length"))
   (width :accessor item-width :initform nil
	  :subelement (distance-element :alias "Width"))
   (weight :accessor item-weight :initform nil
	   :subelement (weight-element :alias "Weight"))
   (list-price :accessor item-list-price :initform nil
	       :subelement (price-element :alias "ListPrice"))
   (title :accessor item-title :initform nil
	  :subelement (simple-text-element :alias "Title"))
   (upc :accessor item-upc :initform nil
	:subelement (simple-text-element :alias "UPC"))
   (isbn :accessor item-isbn :initform nil
	 :subelement (simple-text-element :alias "ISBN"))
   (binding :accessor binding :initform "" :initarg :binding)
   (creators :accessor creators :initform nil :initarg :creators)
   (actors :accessor actors :initform () :initarg :actors)
   (directors :accessor directors :initform nil :initarg :directors)
   (number-of-items :accessor item-number-of-items :initform nil
		    :subelement (numerical-text-element :alias "NumberOfItems"))
   (manufacturer :accessor manufacturer :initform "" :initarg :manufacturer
		 :subelement (simple-text-element :alias "Manufacturer"))
   (product-group :accessor item-product-group :initform nil
		  :subelement (simple-text-element :alias "ProductGroup")))

  (:metaclass element-class)
  (:tags "ItemAttributes")
  (:documentation "HTTPHeader element in Amazon ECS response"))


(defclass offers (xml:xml-serializer)
  ((totaloffers :accessor total-offers :initform nil :initarg :total-offers)
   (totalofferpages :accessor lowest-offer-pages :initform nil :initarg :total-offer-pages)
   (offers :accessor offers :initform () :initarg :offers))
  (:documentation "Summary of offers for a particular item"))

(defclass offer (xml:xml-serializer)
  ((merchant :accessor merchant :initform nil :initarg :merchant)
   (offerattributes :accessor offer-attributes :initform nil :initarg :offer-attributes)
   (seller :accessor seller :initform nil :initarg :seller)
   (offerlisting :accessor offer-listing :initform () :initarg :offer-listing))
  (:documentation "Summary of offers for a particular item"))

(defclass vendor-like-mixin (xml:xml-serializer)
  ((averagefeedbackrating :accessor average-feedback-rating :initform nil :initarg :average-feedback-rating)
   (totalfeedback :accessor total-feedback :initform nil :initarg :total-feedback))
  (:documentation "Mixed into seller and vendor to provide shared slots for the most part"))

(defclass merchant (vendor-like-mixin)
  ((merchantid :accessor merchant-id :initform nil :initarg :merchant-id)
   (glancepage :accessor glance-page :initform nil :initarg :glance-page))
  (:documentation "Summary of offers for a particular item"))

(defclass seller (vendor-like-mixin)
  ((sellerid :accessor seller-id :initform nil :initarg :seller-id))
  (:documentation "Summary of offers for a particular item"))

(defclass offer-attributes (xml:xml-serializer)
  ((condition :accessor offer-condition :initform nil :initarg :condition)
   (conditionnote :accessor condition-note :initform nil :initarg :condition-note)
   (willshipexpedited :accessor will-ship-expedited :initform nil :initarg :will-ship-expedited)
   (willshipinternational :accessor will-ship-international :initform nil :initarg :will-ship-international)
   (subcondition :accessor subcondition :initform nil :initarg :subcondition))
  (:documentation "Summary of offers for a particular item"))
(xml:def-element-class-name OfferAttributes offer-attributes)

(defclass offer-listing (xml:xml-serializer)
  ((offerlistingid :accessor offer-listing-id :initform nil :initarg :offer-listing-id)
   (price :accessor price :initform nil :initarg :price)
   (availability :accessor availability :initform nil :initarg :availability)
   (availabilityattributes :accessor availability-attributes :initform nil :initarg :availability-attributes)
   (amountsaved :accessor amount-saved :initform nil :initarg :amount-saved)
   (percentagesaved :accessor percentage-saved :initform nil :initarg :percentage-saved)
   (exchangeid :accessor exchange-id :initform nil :initarg :exchange-id)
   (quantity :accessor quantity :initform nil :initarg :quantity)
   (iseligibleforsupersavershipping :accessor is-eligible-for-super-saver-shipping :initform nil :initarg :saver-shipping))
  (:documentation "Summary of offers for a particular item"))
(xml:def-element-class-name OfferListing offer-listing)

(defclass availability-attributes (xml:xml-serializer)
  ((availabilitytype :accessor author-element :initform nil :initarg :author-element)
   (minimumhours :accessor title-element :initform "" :initarg :title-element)
   (maximumhours :accessor isbn :initform "" :initarg :isbn))
  (:documentation "HTTPHeader element in Amazon ECS response"))
(xml:def-element-class-name AvailabilityAttributes availability-attributes)

(defclass alternate-version (item-attributes)
  ((asin :accessor amazon-asin :initform nil :initarg :amazon-asin))
  (:documentation "Alternate version of an item."))
(xml:def-element-class-name AlternateVersion alternate-version)

(defclass alternate-versions (xml:xml-serializer)
  ((alternateversions :accessor alternate-version :initform () :initarg :alternate-versions))
  (:documentation "Alternate version of an item."))
(xml:def-element-class-name AlternateVersions alternate-versions)

(defclass creator (xml:xml-serializer)
  ((role :accessor role :initform "" :initarg :role))
  (:documentation ""))