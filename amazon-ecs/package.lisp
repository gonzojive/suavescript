(defpackage :org.iodb.amazon.ecs
  (:nicknames :amazon-ecs :ecs :iodb-ecs)
  (:use :common-lisp :net.telent.date :xml-mop)
  (:export +amazon-merchant-id+ abstract-root-response actors alternate-versions
 amazon-error amazon-errors amazon-item amazon-query-component amount
 amount-saved arg-name arg-value argument-name arguments associate-id author
 authors availability average-feedback-rating aws-key base-ecs-url binding
 category code condition-note conditionnote content control-str creator
 creators currency-code date-element delimiter delivery-method detail-page-url
 dewey-decimal-number digital-distance-element dimension-height
 dimension-length dimension-weight dimension-width dimensional-element
 directors distance-element distance-units ean edition editorial-review
 editorial-review-collection eligible-for-saver-shipping
 eligible-for-saver-shipping? error-code error-message errors exchange-id
 exports features formatted-price generate-ecs-url generate-exports glance-page
 glancepage headers height http-headers hyphen-word hyphenated->camelized
 id-type image-element image-height image-set image-set-category
 image-set-collection image-set-item-medium-image image-set-large-image
 image-set-small-image image-sets image-url image-width is-valid isbn
 ispu-postal-code item-asin item-attributes item-binding item-detail-page-url
 item-dewey-decimal-number item-ean item-edition item-features item-height
 item-id item-image-sets item-isbn item-label item-large-image item-length
 item-like-thing item-list-price item-lookup-request item-lookup-response
 item-medium-image item-number-of-items item-number-of-pages item-offers
 item-package-dimensions item-price-description-mixin item-product-group
 item-publication-date item-publisher item-search-request item-search-response
 item-small-image item-studio item-title item-upc item-weight item-width items
 items-errors items-request-info join-string-list key-value-element keywords
 label language-code large-image list-price lowest-collectible-price
 lowest-new-price lowest-refurbished-price lowest-used-price manufacturer
 medium-image merchant merchant-id merchantid message name number-of-items
 number-of-pages numerical-measurement-element numerical-text-element offer
 offer-attributes offer-condition offer-editorial-reviews offer-listing
 offer-listing-id offer-merchant offer-summary offerlistingid offers
 offers-total-offers offers-total-pages operation operation-arguments
 operation-http-headers operation-request operation-requestid
 package-dimensions percentage-saved price price-amount price-currency-code
 price-element price-formatted product-group publication-date publisher
 quantity reading-level release-date request request-errors request-id
 request-is-valid request-item-id request-item-search-request
 request-processing-time request-response-group requestid response-group
 response-groups response-items response-operation-request response-xmlns
 review-content review-source reviews role sales-rank search-index searchindex
 seller seller-id sellerid simple-text-element small-image source string-list
 studio subcondition summary-total-new sym sym1 sym2 title total-collectible
 total-feedback total-offer-pages total-offers total-pages total-refurbished
 total-results total-used totalnew true-url-key units upc url url-key-form
 url-key-forms url-key-sym url-key-value urlize-keys value vendor-like-mixin
 version weight weight-element width will-ship-expedited
 will-ship-international words xmlns yes-no-element

;;; CUSTOM
 parse-response-stream perform-amazon-search official-amazon-offer?
 item-official-amazon-offer item-authors perform-amazon-request
 price-in-cents price-in-dollars))

(in-package :org.iodb.amazon.ecs)

;;used this to generate export list:
;(do-symbols (sym :org.iodb.amazon.ecs)
;       (if (eql (find-package :org.iodb.amazon.ecs) (symbol-package sym))
;;         (export sym)
;	   (format t "~A~%" sym)))
; sorted via (lambda (sym1 sym2) (string-lessp (string sym1) (string sym2))))

(defun generate-exports ()
  (let ((exports nil))
    (do-symbols (sym :org.iodb.amazon.ecs)
      (when (eql (find-package :org.iodb.amazon.ecs) (symbol-package sym))
	  (push sym exports)))
    (sort exports #'(lambda (sym1 sym2) (string-lessp
					 (string sym1) (string sym2))))))