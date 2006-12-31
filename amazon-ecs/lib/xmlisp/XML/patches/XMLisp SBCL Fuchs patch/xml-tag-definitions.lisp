;;;-*- Mode: Lisp; Package:  xml -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L   T A G   D E C L A R A T I O N S            *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2001, Agentsheets Inc.                       *
;* Filename  : xml-tag-definitions.lisp                              *
;* Updated   : 09/07/01                                              *
;* Version   :                                                       *
;*    1.0    : 04/21/00 defined DEFXMLELEMENT macro                  *
;*    2.0    : 01/22/01 allow xml tag definitions to be associated   *
;*                      with an XML document; added accessors        *
;*    2.1    : 09/07/01 error checking in get-xml-element-definition *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Definition of valid XML elements.                     *
;*********************************************************************

(in-package :xml)

(defvar *XML-ELEMENT-DEFINITIONS* (make-hash-table :size 100))
(defvar *XML-DOCUMENT* nil "The XML document for which the tag definitions need to be declared")

;__________________________________
; M A C R O S                     /
;________________________________/

(defmacro IN-XML-DOCUMENT (Name)
  (let ((Name-var (gensym)))
    `(let ((,Name-var ,(if (stringp Name) 
                         `(intern ,Name)
                         Name)))
       (setq *Xml-Document* ,Name-Var)
       (unless (gethash ,Name-var *Xml-Element-Definitions*)
         (setf (gethash ,Name-var *Xml-Element-Definitions*)
               (make-hash-table :size 100))))))


(defmacro DEFXMLELEMENT (Name &rest Parameters)
  (let ((Name-var (gensym)))
    `(let ((,Name-var ,(if (stringp Name) 
                         `(intern ,Name :xml)
                         Name)))
       (setf (gethash ,Name-var (gethash *Xml-Document* *XML-Element-Definitions*))
             (make-instance 'xml-element-definition
               :name ,Name-var
               :element-attributes (getf ',Parameters :element-attributes)
               :content (getf ',Parameters :content)
               :start-tag-actions (getf ',Parameters :start-tag-actions)
               :end-tag-actions (getf ',Parameters :end-tag-actions)
               :container (getf ',Parameters :container))))))


;__________________________________
; A C C E S S O R S               /
;________________________________/

(defun GET-XML-ELEMENT-DEFINITION (Element-Name)
  (let ((Document-Definition-Hash-Table (gethash *Xml-Document* *Xml-Element-Definitions*)))
    (if Document-Definition-Hash-Table
      (gethash Element-Name Document-Definition-Hash-Table)
      (error "Cannot find XML definition for element ~A. Please make sure that there is an element definition for that element and try again." *Xml-Document*))))

(defun SET-XML-ELEMENT-DEFINITION (Element-Name New-Definition)
  (setf (gethash Element-Name (gethash *Xml-Document* *Xml-Element-Definitions*)) New-Definition))



#| Example tag definitions for worksheet (found in Project Conversion:xml-elements.lisp:

(in-xml-document "worksheet")

(defxmlelement "worksheet"
  :element-attributes (version application x y height width name)
  :content ((xml-element agent))
  :start-tag-actions (agents:create-agentsheets-worksheet)
  :end-tag-actions (agents:finish-worksheet-setup)
  :container nil)

(defxmlelement "agent"
  :element-attributes (name active x y color direction depiction)
  :content ((xml-element attribute))
  :end-tag-actions (agents:add-agent-to-worksheet)
  :container worksheet)

(defxmlelement "attribute"
  :element-attributes (name value)
  :container agent)


|#