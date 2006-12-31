;;-*- Mode: Lisp; Package: xml -*-
;*********************************************************************
;*                                                                   *
;*  PROGRAM:    E L E M E N T    I M P L E M E N T A T I O N         *
;*                                                                   *
;*********************************************************************
;* Author      : Andri Ioannidou (andri@agentsheets.com)             *
;*               http://www.agentsheets.com                          *
;* Copyright   : (c) 1996-2000, Agentsheets Inc.                     *
;* Filename    : xml-element-implementation.lisp                     *
;* Updated     : 05/10/10                                            *
;* Version     :                                                     *
;*    1.0      : 04/21/00  Read-, Get- and Set-Element-Attributes    *
;*    1.1      : 05/10/10  Get-Child-Element, text content accessors *
;* HW/SW       : PowerPC G4, MCL 4.2 & 3.3                           *
;* Abstract    : Element holds the information of an XML element     *
;*********************************************************************

(in-package :xml)

;***************************************
;*  I M P L E M E N T A T I O N        *
;***************************************

(defmethod INITIALIZE-INSTANCE ((Self Xml-Element) &rest Initargs)
  (setf (name Self) (getf Initargs :name))
  (setf (element-attributes Self) (getf Initargs :element-attributes))
  (setf (content Self) (getf Initargs :content))
  (setf (container Self) (getf Initargs :container)))


(defmethod GET-ELEMENT-ATTRIBUTE ((Self Xml-Element) Name)
  (getf (element-attributes Self) Name))


;; how to fix reverse attribute order problem? => use do-reverse-list??
(defmethod SET-ELEMENT-ATTRIBUTE ((Self Xml-Element) Name Value)
  (setf (getf (element-attributes Self) Name) Value))


(defmethod GET-CHILD-ELEMENT ((Self Xml-Element) Name)
  ;; check if content is indeed elements?
  (dolist (Child (content Self))
    (when (equal (name Child) Name) 
      (return-from GET-CHILD-ELEMENT Child)))
  nil)


(defmethod GET-ELEMENT-TEXT-CONTENT ((Self Xml-Element))
  ;; check if content is indeed text?
  (content Self))

(defmethod SET-ELEMENT-TEXT-CONTENT ((Self Xml-Element) Text)
  ;; check if content is indeed text?
  (setf (content Self) Text))
