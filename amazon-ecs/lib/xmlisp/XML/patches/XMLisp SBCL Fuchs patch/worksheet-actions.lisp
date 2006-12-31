;;;-*- Mode: Lisp; Package:  agents -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L   E L E M E N T    A C T I O N S             *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2001, Agentsheets Inc.                       *
;* Filename  : xml-element-actions.lisp                              *
;* Updated   : 04/24/01                                              *
;* Version   :                                                       *
;*    1.0    : 04/18/01 implemented worksheet element action         *
;*    1.1    : 04/24/01 implemented agent element action             *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Implements xml-element actions performed before and   *
;*             after reading the elements                            *
;*********************************************************************

(in-package :agents)


(export '(CREATE-AGENTSHEETS-WORKSHEET
          ADD-AGENT-TO-WORKSHEET
          FINISH-WORKSHEET-SETUP))

;__________________________________
;  W O R K S H E E T     actions  /
;________________________________

(defparameter *Worksheet-In-Progress* nil)
(defparameter *Agent-Attributes-Hash-Table* nil)


(defun CREATE-AGENTSHEETS-WORKSHEET (Worksheet-Element)
  (declare (ignore Worksheet-Element))
  (setq *Worksheet-In-Progress* (link-agentsheet 'new :window-show nil))
  (aim *Worksheet-In-Progress* 'pathname xml:*Xml-File-Being-Parsed*))


(defun FINISH-WORKSHEET-SETUP (Worksheet-Element)
  (let ((Worksheet-Element-Attributes (xml:element-attributes Worksheet-Element))
        (Worksheet-Element-Attributes-Hash-Table (make-hash-table :size 10)))
    (do ((i 0 (+ i 2)))
        ((>= i (length Worksheet-Element-Attributes)))
      (setf (gethash (nth i Worksheet-Element-Attributes) Worksheet-Element-Attributes-Hash-Table) (nth (+ i 1) Worksheet-Element-Attributes)))
    (aim *Worksheet-In-Progress* 'title (concatenate 'string
                                                     "Worksheet: " 
                                                     (string-right-trim ".ws" (gethash 'xml::name Worksheet-Element-Attributes-Hash-Table))))
    (aim *Worksheet-In-Progress* 
         'viewport-size
         (+ 40 (read-from-string (gethash 'xml::width Worksheet-Element-Attributes-Hash-Table)))
         (+ 15 (read-from-string (gethash 'xml::height Worksheet-Element-Attributes-Hash-Table))))
    (aim *Worksheet-In-Progress* 
         'viewport-position
         (read-from-string (gethash 'xml::x Worksheet-Element-Attributes-Hash-Table))
         (read-from-string (gethash 'xml::y Worksheet-Element-Attributes-Hash-Table)))
    (aim *Worksheet-In-Progress* 'appear)))


(defun ADD-AGENT-TO-WORKSHEET (Agent-Element)
  (let ((Agent-Element-Attributes (xml:element-attributes Agent-Element))
        (Agent-Element-Attributes-Hash-Table (make-hash-table :size 10))
        (Agent-Attributes (build-attribute-list (xml:content Agent-Element))))
    (do ((i 0 (+ i 2)))
        ((>= i (length Agent-Element-Attributes)))
      (setf (gethash (nth i Agent-Element-Attributes) Agent-Element-Attributes-Hash-Table) (nth (+ i 1) Agent-Element-Attributes)))
    (let* ((Agent-Type (intern (string-upcase (gethash 'xml::name Agent-Element-Attributes-Hash-Table))))
           (Row (read-from-string (gethash 'xml::y Agent-Element-Attributes-Hash-Table)))
           (Column (read-from-string (gethash 'xml::x Agent-Element-Attributes-Hash-Table)))
           (Level 0)
           (Depiction-String (gethash 'xml::depiction Agent-Element-Attributes-Hash-Table))
           (Depiction (when Depiction-String (intern (string-upcase Depiction-String))))
           (Active-P (if (gethash 'xml::active Agent-Element-Attributes-Hash-Table) nil t))
           (Color-String (gethash 'xml::color Agent-Element-Attributes-Hash-Table))
           (Color (when Color-String (read-from-string Color-String))))
      (xml:format-if-verbose t "Adding the ~A agent to worksheet.." Agent-Type)
      (aim *Worksheet-In-Progress* 'add-agent-from-xml-file Agent-Type Row Column Level Depiction Active-P Color Agent-Attributes))))


(defmethod ADD-AGENT-FROM-XML-FILE (Agent-Type Row Column Level Depiction Active-P Color Attributes)
  (instance-method-of AGENTSHEET)
  (let ((Agent (create-anonymous-instance Agent-Type)))
    (xml:format-if-verbose t "Agent=~A Row=~A Column=~A Level=~A Depiction=~A Active=~A Color=~A Attributes=~A" 
                           (aim Agent 'instance-of)
                           Row Column Level Depiction Active-P Color Attributes)
    (push Agent (agent-ref Agents Row Column 0))
    (when Active-P
      (self 'add-active-agent Agent))
    (self 'forward Row Column 0 1 1 'init)
    (if Depiction 
      (self 'forward Row Column 0 1 1 'depiction Depiction :No-Window-Update t)
      (self 'forward Row Column 0 1 1 'depiction Agent-Type :No-Window-Update t))
    ;;(when Attributes (instance-set-instance-variables Agent Attributes))
    (self 'forward Row Column 0 1 1 'restore-agent-from-file)))


(defun BUILD-ATTRIBUTE-LIST (Attribute-Elements)
  )