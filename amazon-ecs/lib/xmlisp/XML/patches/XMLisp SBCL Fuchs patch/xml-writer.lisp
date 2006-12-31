;;;-*- Mode: Lisp; Package:  xml -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L   W R I T E R                                *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2000, Agentsheets Inc.                       *
;* Filename  : xml-writer.lisp                                       *
;* Updated   : 09/22/00                                              *
;* Version   :                                                       *
;*    1.0    : 09/11/00 defined WRITE-XML-FILE                       *
;*    1.1    : 09/21/00 defined PRINT-OBJECT for Xml-Element         *
;*    1.1.1  : 09/22/00 pretty printing (indented content)           *
;*    1.2    : 05/07/01 fixed printing of textual content            *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Writes XML files.                                     *
;*********************************************************************

(in-package :xml)

(defmethod PRINT-OBJECT ((Self Xml-Element) Stream)
  (declare (special $tab-level$))
  (let ((Attributes (element-attributes Self))
        (Content (content Self))
        (Element-Name (string-downcase (symbol-name (xml::name Self)))))
    (unless (boundp '$tab-level$)
      (setq $tab-level$ 0))
    ;; start tag: element name
    (format Stream "<~A" Element-Name)
    ;; attributes
    (when Attributes
      (do ((index 0 (+ index 2)))
          ((>= index (length Attributes)))
        (format Stream " ~A=~S" 
                (string-downcase (symbol-name (nth index Attributes)))
                (nth (1+ index) Attributes))))
    ;; end tag
    (cond (Content
           (cond ((listp Content)
                  (format Stream ">~C~C" #\Return #\Linefeed)
                  (incf $tab-level$ 3)
                  (dolist (Xml-Subelement Content)
                    (print-n-spaces $tab-level$ Stream)
                    (format Stream "~A~C~C" Xml-Subelement #\Return #\Linefeed))
                  (print-n-spaces (decf $tab-level$ 3) Stream))
                 (t (format Stream ">~A" Content)))
           (format Stream "</~A>" Element-Name))
          (t (format Stream "/>")))))


(defmethod WRITE-ELEMENT ((Self XML-Element) Destination-File)
  (with-open-file (File Destination-File 
                        :direction :output
                        ;; :if-does-not-exist :create
                        :if-exists :supersede)
    (pprint Self File)))
