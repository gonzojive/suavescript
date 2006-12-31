;;;-*- Mode: Lisp; Package:  xml -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L   U T I L I T I E S                          *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2000, Agentsheets Inc.                       *
;* Filename  : xml-utilities.lisp                                    *
;* Updated   : 04/24/01                                              *
;* Version   :                                                       *
;*    1.0    : 04/23/01 moved printing stuff here from other files   *
;*    1.1    : 04/24/01 implemented map-from-xml                     *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Utility functions for printing and maping to and from *
;*             XML names.                                            *
;*********************************************************************

(in-package :xml)

;__________________________________
; P A R A M E T E R S             /
;________________________________/

(defparameter *PRINT-VERBOSE* nil "Variable for printing debugging messages. If true, then the messages are printed. If nil, then the messages are not printed.")
(defparameter *XML-PROLOG* "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" "the XML file prolog")   ;; used to be "<?xml version=\"1.0\"?>"
(defparameter *XML-VERSION* "2.0" "the XML version of the AgentSheets files")
(defparameter *XML-FILE-VERSION* "1.0" "XML version")
(defparameter *XML-FILE-BEING-PARSED* nil)


;__________________________________
; P R I N T I N G                 /
;________________________________/

(defun FORMAT-IF-VERBOSE (Destination Control-String &rest Arguments)
  (when *print-verbose*
    (terpri)
    (apply #'format Destination Control-String Arguments)))

;; store values as strings or just print as strings?

(defun PRINT-N-SPACES (N Stream)
  (dotimes (i N) (princ #\space Stream)))


;__________________________________
; N A M E    C O N V E R S I O N  /
;________________________________/

(defun MAP-TO-XML (String) "
  Convert String to an XML-compatible string:
   \"<\" becomes \"&lt;\"
   \">\" becomes \"&gt;\"
   \"&\" becomes \"&amp\"
   \" becomes \"&quot;\"
   and the newline character becomes \"&#10;\""
  (when String
    (with-output-to-string (Output)
      (with-input-from-string (Input String)
        (loop
          (let ((Char (or (read-char Input nil nil) (return))))
            (case Char
              (#\< (princ "&lt;" Output))
              (#\> (princ "&gt;" Output))
              (#\& (princ "&amp;" Output))
              (#\newline (princ "&#10;" Output))
              (#\" (princ "&quot;" Output))
              (t (princ Char Output))))))
      Output)))


(defun MAP-FROM-XML (String) "
  Convert String from an XML-compatible string to a regular string: 
   \"&lt;\" becomes \"<\"
   \"&gt;\" becomes \">\" 
   \"&amp\" becomes \"&\"
   \"&quot;\" becomes  \"
   and \"&#10;\" becomes the newline character"
  (when String
    (with-output-to-string (Output)
      (with-input-from-string (Input String)
        (loop
          (let ((Char (or (read-char Input nil nil) (return))))
            (case Char
              (#\& (let ((Next-Char (or (read-char Input nil nil) (return))))
                     (case Next-Char
                       (#\l (let ((Next-Char1 (or (read-char Input nil nil) (return)))
                                  (Next-Char2 (or (read-char Input nil nil) (return))))
                              (when (and (eq  Next-Char1 #\t) (eq  Next-Char2 #\;))
                                (princ #\< Output))))
                       (#\g (let ((Next-Char1 (or (read-char Input nil nil) (return)))
                                  (Next-Char2 (or (read-char Input nil nil) (return))))
                              (when (and (eq  Next-Char1 #\t) (eq  Next-Char2 #\;))
                                (princ #\> Output))))
                       (#\a (let ((Next-Char1 (or (read-char Input nil nil) (return)))
                                  (Next-Char2 (or (read-char Input nil nil) (return)))
                                  (Next-Char3 (or (read-char Input nil nil) (return))))
                              (when (and (eq  Next-Char1 #\m) (eq  Next-Char2 #\p) (eq  Next-Char3 #\;))
                                (princ #\& Output))))
                       (#\# (let ((Next-Char1 (or (read-char Input nil nil) (return)))
                                  (Next-Char2 (or (read-char Input nil nil) (return)))
                                  (Next-Char3 (or (read-char Input nil nil) (return))))
                              (when (and (eq  Next-Char1 #\1) (eq  Next-Char2 #\0) (eq  Next-Char3 #\;))
                                (princ #\newline Output))))
                       (#\q (let ((Next-Char1 (or (read-char Input nil nil) (return)))
                                  (Next-Char2 (or (read-char Input nil nil) (return)))
                                  (Next-Char3 (or (read-char Input nil nil) (return)))
                                  (Next-Char4 (or (read-char Input nil nil) (return))))
                              (when (and (eq  Next-Char1 #\u) (eq  Next-Char2 #\o) (eq  Next-Char3 #\t) (eq  Next-Char4 #\;))
                                (princ #\" Output))))        
                       (t (return)))))
              (t (princ Char Output))))))
      Output)))

