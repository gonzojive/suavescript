;;-*- Mode: Lisp; Package: xml -*-
;*********************************************************************
;*                                                                   *
;*  PROGRAM:    E L E M E N T    D E F I N I T I O N                 *
;*                                                                   *
;*********************************************************************
;* Author      : Andri Ioannidou (andri@agentsheets.com)             *
;*               http://www.agentsheets.com                          *
;* Copyright   : (c) 1996-2000, Agentsheets Inc.                     *
;* Filename    : xml-element-definition.lisp                         *
;* Updated     : 04/20/00                                            *
;* Version     :                                                     *
;*    1.0      : 04/20/00  Element class definition and API spec     *
;*    1.1      : 04/23/01  Changed action specs                      *
;* HW/SW       : PowerPC G4, MCL 4.2 & 3.3                           *
;* Abstract    : Element holds the information of an XML element     *
;*********************************************************************

(in-package :xml)

;***************************************
;*  C L A S S   D E F I N I T I O N S  *
;***************************************

(defclass XML-ELEMENT ()
  ((name :accessor name :initarg :name)
   (element-attributes :accessor element-attributes :initarg :element-attributes :initform nil)
   (content :accessor content :initarg :content :initform nil)
   (container :accessor container :initarg :container :initform nil))
  (:documentation "Element holds the information in an XML element"))


(defclass XML-ELEMENT-DEFINITION ()
  ((name :accessor name :initarg :name)
   (element-attributes :accessor element-attributes :initarg :element-attributes)
   (content :accessor content :initarg :content)
   (start-tag-actions :accessor start-tag-actions :initarg :start-tag-actions)
   (end-tag-actions :accessor end-tag-actions :initarg :end-tag-actions)
   (container :accessor container :initarg :container))
  (:documentation "Element holds the information about an XML element"))



;***************************************
;*  A P I                              *
;***************************************

(defgeneric READ-ELEMENT (Xml-Element Stream)
  (:documentation "Reads the element"))

(defgeneric READ-ATTRIBUTES (Xml-Element Stream)
  (:documentation "Reads the attributes of the xml element. Uses the definition of the XML element to decide which attributes to read. Attributes are of the form AttributeName=\"AttributeValue\""))

(defgeneric READ-CONTENT (Xml-Element Stream)
  (:documentation "Reads the content of the xml element: another element, character data, a reference, a CDATA section, a processing instruction or a comment."))

(defgeneric GET-ELEMENT-ATTRIBUTE (Xml-Element Attribute-Name)
  (:documentation "Gets the value of XML-Element's attribute named Attribute-Name"))

(defgeneric SET-ELEMENT-ATTRIBUTE (Xml-Element Attribute-Name Attribute-Value)
  (:documentation "Sets the value of XML-Element's attribute named Attribute-Name to Attribute-Value"))

(defgeneric WRITE-ELEMENT (Xml-Element File)
  (:documentation "Writes the element to the File specified"))
