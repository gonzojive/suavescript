;;-*- Mode: Lisp; Package: XML -*-
;*********************************************************************
;*                                                                   *
;*            X M L i s p                                            *
;*                                                                   *
;*********************************************************************
;* Author       : Alexander Repenning, alexander@agentsheets.com     *
;*                http://www.agentsheets.com                         *
;* Copyright    : (c) 1996-2004, AgentSheets Inc.                    *
;* Filename     : XMLisp.lisp                                        * 
;* Last Update  : 02/15/05                                           *
;* Version      :                                                    *
;*    1.0       : 09/19/04                                           *
;*    1.1       : 09/30/04 encode/decode strings in XML              *
;*    1.1.1     : 10/01/04 subobjects can be aggregated as arrays    *
;*    1.2       : 10/09/04 abreviated printing to inspector/listener *
;*                         serialization includes arrays             *
;*                         <?xml .. ?> headers                       *
;*                         :type slot interpretation                 *
;*    1.3       : 10/11/04 content only tags, e.g., <a>bla</a>       *
;*    1.4       : 10/12/04 SGML tags: <--, <![CDATA[, <!DOCTYPE      *
;*    1.4.1     : 10/16/04 print-slots                               *
;*    1.4.2     : 10/19/04 finished-reading-attributes               *
;*    1.4.3     : 10/28/04 ignore white space after name             *
;*    1.4.4     : 11/12/04 type character added                      *
;*    1.5       : 12/09/04 element and class name can be different   *
;*                         CODECs for typed-attribute-value          *
;*    1.6       : 12/16/04 read error: open file in Fred at position *   
;*    1.6.1     : 12/22/04 print-typed-subelement-value              *
;*    2.0       : 02/10/05 use real MOP, allow compilation           *
;*    2.0.1     : 02/15/05 OpenMCL support, save-object              *
;* Systems      : G4, OS X 10.3.8                                    *
;* Lisps        : MCL 5.0, MCL 5.1b4, LispWorks 4.3.7, OpenMCL 0.14  *
;* Licence      : LGPL                                               * 
;* Uses         : XML by Andri Ioannidou                             *
;* Abstract     : Integrate XML reading/writing with Lisp            *
;*   To use XMLisp mix in xml-serializer class into your class.      *
;*   When lisp reader sees: <bla x="13" y="20"> it will              *
;*    - create an instance of class "BLA"                            *
;*    - set slot "X" to 13 and slot "Y" to 20                        * 
;*    - if slot includes :type use CODEC                             *
;*   Objects can have subobjects. Aggregation can be controlled      *
;*   by redefining aggregator functions, e.g., add-subobject         *
;*                                                                   *
;*********************************************************************

(in-package :xml)

(defvar *XMLisp-Output-Non-Default-Value-Attributes* nil "if non-nil output also attributes that are the same as the default value.")

(defvar *XMLiSP-Element-Class-Names* (make-hash-table :test #'eq) "table mapping element names to class names")

;*******************************
; XML Serializer class         *
;*******************************

(defclass XML-SERIALIZER ()
  ((content :accessor content :initarg :content :initform nil :documentation "content not wrapped up as tag or attribute, e.g. the link name of <a> tag"))
  (:documentation "Mixin to serialize objects as XML looking things"))


(defgeneric SAVE-OBJECT (Xml-Serializer Filename &key Verbose If-Exists Xml-Header)
  (:documentation "Save object into <Filename>. By default add a valid XML header"))


(defgeneric SET-ATTRIBUTE-VALUE (Xml-Serializer Attribute-Name Value)
  (:documentation "Set the value of an attribute. Default: find slot matching <attribute-name> and set its value to <value>"))


(defgeneric ADD-SUBOBJECT (Xml-Serializer Subobject)
  (:documentation "Add a subobject. Default: If subobject is of type bla and there is a slot called bla assign it to that slot. If subobject is of type bla and there is a slot called blas then add bla as element of a list to slot blas."))


(defgeneric ADD-OBJECT-TO-SLOT (Xml-Serializer Object Slot-Name)
  (:documentation "Add object to slot <slot-name>. Default: nconc object to end of list, not good for large lists but preserves reading order."))


(defgeneric FIND-SLOT-DEFINITION (Xml-Serializer Name)
  (:documentation "Return slot defnition matching <Name>"))


(defgeneric CLEANUP-SUB-OBJECT-SLOTS (Xml-Serializer Slot-Names)
  (:documentation "Called after all the sub objects have been added"))


(defgeneric XML-PRINTABLE-AS-SUBELEMENT-P (Xml-Serializer)
  (:documentation "True if printable as subelement <bla .../>"))

(defgeneric XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P (Xml-Serializer)
  (:documentation "True if printable as attribute value bla=\"???\""))


(defgeneric PRINT-TYPED-ATTRIBUTE-VALUE (Value Type Stream)
  (:documentation "Encode attribute <value> into an external XML compliant represetation and print into <stream>"))


(defgeneric READ-TYPED-ATTRIBUTE-VALUE (Value Type)
  (:documentation "Return decoded XML <value> of <type>. "))


(defgeneric PRINT-TYPED-SUBELEMENT-VALUE (Value Type Stream)
  (:documentation "Encode attribute <value> into an external XML compliant represetation and print into <stream>"))


(defgeneric MAP-OBJECT (Collection Function)
   (:documentation "If <collection> is a structured object such as a string, list or array call <function> with each element"))


(defgeneric PRINT-SLOTS (Xml-Serializer)
  (:documentation "List of slot names to be printed. Return nil to print no slots, :all to print all. Slots will still be excluded when print-slot-with-name-p returns nil"))


(defgeneric PRINT-SLOT-WITH-NAME-P (Xml-Serializer Name)
   (:documentation "Return true if slot with <name> should be printed. Default: t. Typical use: avoid recursion"))


(defgeneric PRINT-SUBELEMENTS-TO-STREAM-P (Xml-Serializer Stream)
  (:documentation "If true then sub elements, if there are any, will be printed into stream"))


(defgeneric PRINT-SLOT-VALUE-AS-ATTRIBUTE (Xml-Serializer Slot Value)
  (:documentation "Print <slot> as attribute of <Value>"))


(defgeneric FINISHED-READING (Xml-Serializer Stream)
  (:documentation "called when done with reading: all attributes and sub elements have been created"))


(defgeneric FINISHED-READING-ATTRIBUTES (Xml-Serializer Stream)
  (:documentation "called when done with reading attributes: sub elements have NOT been created"))

;____________________________
;  Element & Class Names     |
;____________________________

(defmacro DEF-ELEMENT-CLASS-NAME (Element-Name Class-Name)
  `(setf (gethash ',Element-Name *XMLISP-Element-Class-Names*) ',Class-Name))


(defun ELEMENT-CLASS-NAME (Element-Name) "
  in:  Element-Name symbol.
  out: Class-Name symbol.
  Return the class name."
 (gethash Element-Name *XMLISP-Element-Class-Names*))


(defun CLASS-ELEMENT-NAME (Class-Name) "
  in:  Class-Name symbol.
  out: Element-Name symbol.
  Return the element-name matching <class-name>."
 (maphash
  #'(lambda (Key Value)
      (when (eq Class-Name Value) (return-from class-element-name Key)))
  *XMLISP-Element-Class-Names*))


;____________________________
;  default implementations   |
;____________________________

;; names and print names

(defmethod XML-TAG-NAME-SYMBOL ((Self xml-serializer))
  (or (class-element-name (type-of Self))
      (type-of Self)))


(defmethod XML-TAG-NAME-STRING ((Self xml-serializer))
  (string-downcase (symbol-name (xml-tag-name-symbol Self))))


;; map objects into their components

(defmethod MAP-OBJECT ((Self xml-serializer) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self sequence) Function)
   (map nil Function Self))

(defmethod MAP-OBJECT ((Self hash-table) Function)
  (maphash #'(lambda (Key Value) (declare (ignore Key)) (funcall Function Value)) Self))

(defmethod MAP-OBJECT ((Self number) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self string) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self symbol) Function)
   (funcall Function Self))

(defmethod MAP-OBJECT ((Self array) Function)
   (let* ((Size (array-total-size Self))
          (Vector (make-array Size :element-type (array-element-type Self) :displaced-to Self)))
     (dotimes (I Size)
       (let ((Element (aref Vector I)))
         (when (xml-printable-as-subelement-p Element)
           (map-object Element Function))))))


;; print which slots and what kinds of values?

(defmethod PRINT-SLOTS ((Self xml-serializer))
  :all)

(defmethod PRINT-SLOT-WITH-NAME-P ((Self xml-serializer) Name)
   (case Name
     (content nil)
     (t t)))


(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self t))    nil) ;; most general case => NO

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self null))    nil)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self xml-serializer))    t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self string))    nil)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self sequence))    t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self array))    t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self hash-table)) t)

(defmethod XML-PRINTABLE-AS-SUBELEMENT-P ((Self list))
   (every #'xml-printable-as-subelement-p Self))


(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self t))    nil) ;; most general case => NO

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self string))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self number))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self character))  t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self symbol))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self list))    t)

(defmethod XML-PRINTABLE-AS-ATTRIBUTE-VALUE-P ((Self pathname))    t)


(defmethod PRINT-SUBELEMENTS-TO-STREAM-P ((Self xml-serializer) Stream)
  (declare (ignore Stream))
  t)

;; finished reading

(defmethod FINISHED-READING ((Self xml-serializer) Stream)
  ;; do nothing
  (declare (ignore Stream))
  )


(defmethod FINISHED-READING-ATTRIBUTES ((Self xml-serializer) Stream)
  ;; do nothing
  (declare (ignore Stream))
  )

;______________________________
; compilation and load forms   |
;______________________________

(defmethod MAKE-LOAD-FORM ((Self xml-serializer) &optional Environment)
  ;; if we want to compile files containing XML expression we better make some load forms
  (make-load-form-saving-slots Self :environment Environment))

;*******************************************
;* User level Error handling               *
;*******************************************

#+(or (not :mcl) :openmcl)
(defun SHOW-ERROR-IN-STREAM-TO-USER (Stream)
  ;; no generic Common Lisp solution
  (declare (ignore Stream))
  )

#+(and :mcl (not :openmcl))
(defun SHOW-ERROR-IN-STREAM-TO-USER (Stream)
  (format t ";; attempting to open file containing error. Error Position: ~A..." (ccl::%fpos (slot-value Stream 'ccl::fblock)))
  ;; Open file in FRED and set cursor to location, scroll if necessary
  (let ((Fred (ed (parse-namestring Stream))))
    (ccl:set-mark (ccl:fred-buffer Fred) (ccl::%fpos (slot-value Stream 'ccl::fblock)))
    (ccl:window-show-cursor Fred)
    (ccl:fred-update Fred)))


;********************************************
;*  Typed Attribute Value CODECs            *
;*    print encoded value into XML stream   *
;*    read decoded XML into internal format *
;********************************************

;_______________________________________
; default printer/reader:               |
;_______________________________________

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE ((Value t) (Type t) Stream)
  ;; (warn "no XML encoder for \"~A\" of type \"~A\"" Value Type)
  (prin1 Value Stream))


(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value t) (Type t))
  (warn "no XML decoder for \"~A\" or type \"~A\"" Value Type)
  Value)

;_____________________________________
; specific printer/reader              |
;_____________________________________

;; SYMBOL

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'symbol)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'symbol)))
  (intern Value))

;; STRING

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'string)) Stream)
  (prin1 (encode-xml-string Value) Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'string)))
  ;; !!! should probably decode the string????
  Value)

;; CHARACTER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'character)) Stream)
  (prin1 (encode-xml-string (string Value)) Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'character)))
  (char Value 0))

;; INTEGER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'integer)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'integer)))
  (parse-integer Value))

;; NUMBER

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'number)) Stream)
  (format Stream "\"~A\"" Value))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'number)))
  (read-from-string Value))


;; BOOLEAN

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'boolean)) Stream)
  (prin1 (if Value "true" "false") Stream))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'boolean)))
  (if (string-equal Value "true") t nil))

;; SHORT-FLOAT

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'short-float)) Stream)
  (format Stream "\"~A\"" (float Value 0.0)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'short-float)))
  (float (read-from-string Value) 0s0))

;; PATHNAME

(defmethod PRINT-TYPED-ATTRIBUTE-VALUE (Value (Type (eql 'pathname)) Stream)
  (declare (ignore Stream))
  (prin1 (convert-to-unix-pathname Value)))

(defmethod READ-TYPED-ATTRIBUTE-VALUE ((Value string) (Type (eql 'pathname)))
  (convert-to-lisp-pathname Value))

;********************************************
;*  Typed Subelement Value CODECs           *
;*    print encoded value into XML stream   *
;********************************************

;_______________________________________
; default printer: warn                |
;_______________________________________

(defmethod PRINT-TYPED-SUBELEMENT-VALUE ((Value t) (Type t) Stream)
  ;; (warn "no XML encoder for \"~A\" of type \"~A\"" Value Type)
  ;; do the same as with untyped subelements: map them
  (map-object 
   Value
   #'(lambda (Object)
       (terpri Stream)
       (print-object Object Stream))))

;*************************************
;*  SGML-TAG    Class                *
;*************************************

(defclass SGML-TAG (xml-serializer)
   ()
   (:documentation "SGML Tag. No sub elements, e.g., <!-- Copyright (C) 2000-2003 - GISUser.com -->"))


(defmethod END-TAG-NAME-STRING ((Self sgml-tag))
   ">")


(defmethod READ-XMLISP-ELEMENT ((Self sgml-tag) Stream)
   (setf (content Self) (read-until-token Stream (end-tag-name-string Self)))  ;; no decoding
   Self)


(defmethod PRINT-OBJECT ((Self sgml-tag) Stream)
  (print-xml-indent Stream)
  (format Stream "<~A ~A~A" (string-upcase (xml-tag-name-string Self)) (content Self) (end-tag-name-string Self)))  ;; no encoding


;*************************************
;*  ![CDATA[    Class                *
;*************************************

(defclass ![CDATA[ (sgml-tag)
  ()
  (:documentation "SGML uninterpreted content only class. Does not encode/decode strings"))


(defmethod END-TAG-NAME-STRING ((Self ![cdata[))
   "]]>")


;*************************************
;*  !DOCTYPE    Class                *
;*************************************

(defclass !DOCTYPE (sgml-tag)
  ()
  (:documentation "SGML metadata"))


;*************************************
;*  !--         Class                *
;*************************************

(defclass !-- (sgml-tag)
  ()
  (:documentation "SGML comment"))


(defmethod END-TAG-NAME-STRING ((Self !--))
   "-->")


;*************************************
;*  xml-content Class                *
;*************************************

(defclass XML-CONTENT (xml-serializer)
   ((name :accessor name :initform "untitled" :initarg :name :documentation "element tag name"))
   (:documentation "Content elements have ONLY content: they may not hold sub element or attribute-based content, e.g., <copyright>Copyright 2004, AgentSheets Inc.</copyright>"))


(defmethod XML-TAG-NAME-SYMBOL ((Self xml-content))
   (name Self))


(defmethod PRINT-SLOT-WITH-NAME-P ((Self xml-content) Name)
   (case Name
     (name nil)
     (t (call-next-method))))

;_____________________________
; MOP hacks                   |
;_____________________________

;; this is the place where to put MOP hacks for different Lisp implementations


#+(and :mcl (not :ccl-5.1))  ;; MCL < 5.1 does not have this MOP function!
(defmethod CLASS-SLOTS ((Class standard-class))
  ;; Art of MOP: p. 214
  ;; pretty slow: don't use this if you don't have to
  (coerce (rest (slot-value Class 'ccl::slots)) 'list))


(defmethod FIND-SLOT-DEFINITION ((Self xml-serializer) Name)
  (find Name (class-slots (class-of Self)) :key #'slot-definition-name))


#+(and :mcl (not :openmcl) (not :ccl-5.1))  ;; the generic version would be very slow with MCL < 5.1
(defmethod FIND-SLOT-DEFINITION ((Self xml-serializer) Name)
  (declare (optimize))
  (let ((Slot-Definitions (rest (slot-value (class-of Self) 'ccl::slots))))
    (dotimes (I (length Slot-Definitions))
      (declare (fixnum i))
      (let ((Slot-Definition (svref Slot-Definitions i)))
        (when (eq (first Slot-Definition) Name) (return Slot-Definition))))))

;_____________________________
; Symbol functions            |
;_____________________________

(defun XMLISP-SYMBOL-NAME (Symbol)
  (string-downcase (symbol-name Symbol)))


(defun MAKE-XMLISP-SYMBOL (Name) "
  in:  Name string.
  out: Symbol symbol.
  Turn <Name> into <Symbol> taking into account the current readtable's case."
  ;; could just use read-from-string but that's super slow
  (intern (ecase (readtable-case *Readtable*)
            (:upcase (string-upcase Name))
            (:downcase (string-downcase Name))
            (:preserve Name)
            (:invert
             (cond
              ((every #'upper-case-p Name) (string-downcase Name))
              ((every #'lower-case-p Name) (string-upcase Name))
              (t Name))))))


;_____________________________
; Pathname conversion         |
;_____________________________

(defun DISK-NAME ()
  (second (pathname-directory (truename "home:"))))


(defun SPLIT-STRING (String Splitter-Char)
  (let ((Start 0)
        (List nil))
    (dotimes (I (length String) List)
      (cond
       ;; splitter char
       ((char= (char String i) Splitter-Char)
        (setq List (append List (list (subseq String Start I))))
        (setq Start (+ i 1)))
       ;; the end
       ((= i (1- (length String)))
        (setq List (append List (list (subseq String Start (1+ i))))))))))


(defun PARSE-FILE-NAME (Name)
  (let ((Dot-Position (position #\. Name)))
    (if Dot-Position
      (values
       (subseq Name 0 Dot-Position)
       (subseq Name (1+ Dot-Position)))
      Name)))


(defun UNIX-PATHNAME-DIRECTORY-P (Unix-Pathname)
  (char= (char Unix-Pathname (1- (length Unix-Pathname))) #\/))


(defun CONVERT-TO-UNIX-PATHNAME (Pathname)
  (with-output-to-string (Unix-Pathname)
    (dolist (Component (rest (rest (pathname-directory Pathname))))
      (format Unix-Pathname "/~A" Component))
    (cond
     ;; directory
     ((or (null (pathname-name Pathname))
          (string-equal (pathname-name Pathname) ""))
      (princ #\/ Unix-Pathname))
     ;; file
     (t
      (format Unix-Pathname "/~A" (pathname-name Pathname))
      (when (pathname-type Pathname)
        (format Unix-Pathname ".~A" (pathname-type Pathname)))))))


(defun CONVERT-TO-LISP-PATHNAME (Unix-Pathname)
  (with-input-from-string (Path Unix-Pathname)
    (unless (char= (read-char Path) #\/) (error "path needs to start with \"/\""))
    (cond
     ((unix-pathname-directory-p Unix-Pathname)
      (make-pathname 
       :directory (append (list :absolute (disk-name)) (rest (split-string Unix-Pathname #\/)))))
     (t
      (let ((Path-List (split-string Unix-Pathname #\/)))
        (multiple-value-bind (Name Extension)
                             (parse-file-name (first (last Path-List)))
          (make-pathname
           :directory (append (list :absolute (disk-name)) (rest (butlast Path-List)))
           :name Name
           :type Extension)))))))

#| Test:

(convert-to-unix-pathname
 (convert-to-lisp-pathname "/Users/alex/Desktop/enemy0.vat"))

(convert-to-unix-pathname
 (convert-to-lisp-pathname "/Users/alex/Desktop/"))


|#
;_____________________________
; low level Read functions    |
;_____________________________


(defvar *XML-Entity-Reference-Table* 
  '(("lt;" #\<) ("gt;" #\>) ("amp;" #\&) ("sq;" #\') ("apos;" #\') ("&dq;" #\") ("quot;" #\") ("#10;" #\newline))
  "http://www.w3.org/TR/WD-xml-961114.html#sec4.1")


(defun DECODE-XML-ENTITY-REFERENCE (Stream) "
  If the XML escape character & has been encountered use this function to decode the rest of the entity reference"
  ;; http://www.w3.org/TR/WD-xml-961114.html#sec4.1
  (let* ((Char (read-char Stream))
         (Entity-Reference (find Char *XML-Entity-Reference-Table* :key #'(lambda (Translator) (char (first Translator) 0)) :test #'char=)))
    (unless Entity-Reference (error "~A is not part of an XML entity referece" Char))
    (dotimes (I (1- (length (first Entity-Reference))) (second Entity-Reference))
      (unless (char= (read-char Stream) (char (first Entity-Reference) (1+ i))) (error "http://www.w3.org/TR/WD-xml-961114.html#sec4.1")))))


(defun READ-UNTIL-TOKEN (Stream Token &key Escape-Char Decode-Function) "
  in:  Stream stream; Token string; &key Escape-Char char; Decode-Function stream->char.
  out: String string.
  Read from stream until token. If there is an escape-char use the decode-funtion to parse it."
  (let ((String (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t))
        (Match 0)
        (End (length Token)))
    (loop
      (let ((Char (read-char Stream)))
        (cond
         ;; Match!
         ((char= Char (char Token Match))
          (incf Match)
          ;; are we done yet?
          (when (= Match End) (return String)))
         ;; NO match
         (t
          ;; resolve partial match
          (dotimes (I Match) 
            (vector-push-extend (char Token i) String))
          (setq Match 0)
          (cond
           ;; escape character that needs decoding? 
           ((and Escape-Char (char= Char Escape-Char) Decode-Function)
            (vector-push-extend (funcall Decode-Function Stream) String))
           ;; legit part of string
           (t 
            (vector-push-extend Char String)))))))))


(defun SKIP-UNTIL-CHARS (Stream &rest Chars) "
  Find all chars in sequence and keep reading until last char of <Chars> is found."
  (dolist (Char Chars)
    (loop
      (when (char= Char (read-char Stream)) (return)))))


(defun ENCODE-XML-STRING (String) "
  Convert String to an XML-compatible string:
   \"<\" becomes \"&lt;\"
   \">\" becomes \"&gt;\"
   \"&\" becomes \"&amp\"
   \" becomes \"&quot;\"
   and the newline character becomes \"&#10;\""
  ;; should use *XML-ENTITY-REFERENCE-TABLE*
  (let ((Output (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t)))
    (with-input-from-string (Input String)
      (loop
        (let ((Char (or (read-char Input nil nil) (return Output))))
          (case Char
            (#\< 
             (vector-push-extend #\& Output)
             (vector-push-extend #\l Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (#\> 
             (vector-push-extend #\& Output)
             (vector-push-extend #\g Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (#\& 
             (vector-push-extend #\& Output)
             (vector-push-extend #\a Output)
             (vector-push-extend #\m Output)
             (vector-push-extend #\p Output)
             (vector-push-extend #\; Output))
            (#\newline 
             (vector-push-extend #\& Output)
             (vector-push-extend #\# Output)
             (vector-push-extend #\1 Output)
             (vector-push-extend #\0 Output)
             (vector-push-extend #\; Output))
            (#\" 
             (vector-push-extend #\& Output)
             (vector-push-extend #\q Output)
             (vector-push-extend #\u Output)
             (vector-push-extend #\o Output)
             (vector-push-extend #\t Output)
             (vector-push-extend #\; Output))
            (t 
             (vector-push-extend Char Output))))))))

;________________________________________
; Token level Reader functions           |
;________________________________________

(defun SKIP-XML-HEADER (Stream) "
  For now we do not do anything with the header content but just make sure we skip it."
 (let ((Char (read-char Stream)))
   (unless (char= Char #\?) (return-from skip-xml-header (unread-char Char Stream)))
   (skip-until-chars Stream #\? #\> #\<)))


(defun READ-XMLISP-NAME (Stream) "
  Valid names start with a letter, _ or :, have to contain letters or digits or other valid characters (see XML spec).
  Extended with SGML spec. allowing names such as'<![CDATA['"
  (let ((Name (make-array 40 :fill-pointer 0 :element-type 'character :adjustable t))
        (Char (read-char Stream)))
    ;; check first character
    (cond
     ((or (letterp Char) (char= Char #\_) (char= Char #\:)
          (char= #\!))  ;; SGML char, <![CADATA[, <!--
      ;; read and check the rest of the characters: stop if you find white space, = or >
      (vector-push-extend Char Name)
      (loop
        (let ((Char (read-char Stream)))
          (cond
           ;; complete: return as symbol
           ((or (white-space-p Char)
                (char= Char #\=)
                (char= Char #\>)
                (char= Char #\/))
            (unread-char Char Stream)
            (return (values (make-xmlisp-symbol Name)
                            Name)))
           ;; part of name
           ((or (namecharp Char)
                (char= Char #\[))  ;; SGML
            (vector-push-extend Char Name))
           ;; trouble
           (t 
            (show-error-in-stream-to-user Stream)
            (error "Character ~C is not a valid character for a name" Char))))))
     (t
      (show-error-in-stream-to-user Stream)
      (error "Not a valid start character for name")))))


(defun READ-XMLISP-VALUE (Stream)
  (if (char= (read-char Stream) #\")
    (read-until-token Stream "\"" :escape-char #\& :decode-function #'decode-xml-entity-reference)
    (progn
      (show-error-in-stream-to-user Stream)
      (error "not a valid XML value"))))


(defmethod READ-XMLISP-CHARACTER-CONTENT ((Self xml-serializer) Stream)
  (prog1
    (read-until-token Stream "<" :escape-char #\& :decode-function #'decode-xml-entity-reference)
    (unread-char #\< Stream)))


(defun READ-NAME-AND-MAKE-INSTANCE (Stream) "
  If name corresponds to an existing class create an instance that of that instance. 
  Search strategy:
  1) look in element-class-name table
  2) look for class with symbol-name matching original case
  3) look for class with symbol-name matching readtable case converted (probably all uppercase) case
  4) create a much more constrained xml-content instance"
  (multiple-value-bind (Symbol String)
                       (read-xmlisp-name Stream)
    (let ((Element-Class-Name (element-class-name Symbol))
          (Original-Case-Symbol (find-symbol String)))
      (cond
       ;; 1) lookup element class name table
       (Element-Class-Name
        ;; if this name is in the table we should interpret lack of class to be an error
        (if (find-class Element-Class-Name nil)
          (make-instance Element-Class-Name)
          (error "element \"~A\" cannot produce instance of class \"~A\" because that class does not exist" String Element-Class-Name)))
       ;; 2) Original Case matches class name
       ((and Original-Case-Symbol (find-class Original-Case-Symbol nil))
        (make-instance Original-Case-Symbol))
       ;; 3) readtable translated case matches class name
       ((find-class Symbol nil)
        (make-instance Symbol))
       ;; 4) xml-content
       (t
        (make-instance 'xml-content :name Symbol))))))


(defmethod READ-XMLISP-ATTRIBUTES ((Self xml-serializer) Stream)
  (loop
    (let ((Char (read-char Stream)))
      (case Char
        ((#\/ #\>) ;; delimiters
         (unread-char Char Stream)
         (return t))
        (t
         (unread-char Char Stream)
         (read-white-space Stream)
         (set-attribute-value Self (prog1 (read-xmlisp-name Stream) (read-equal-sign Stream)) (read-xmlisp-value Stream))
         (read-white-space Stream))))))


(defmethod READ-XMLISP-ELEMENT-CONTENT ((Self xml-serializer) Stream)
  (let ((Char (read-char Stream)))
    (case Char
      ;; found an empty element or the end of this element
      (#\/ 
       (unread-char Char Stream)
       (return-from read-xmlisp-element-content nil))
      ;; start a new sub element
      (t 
       (unread-char Char Stream)
       (add-subobject Self (read-xmlisp-element (read-name-and-make-instance Stream) Stream))
       Self))))


(defmethod READ-XMLISP-END-TAG ((Self xml-serializer) Stream)
  (let ((End-Tag (read-xmlisp-name Stream)))
    (read-white-space Stream)
    (case (read-char Stream)
      (#\> ;; match tags
       (if (eq (xml-tag-name-symbol Self) End-Tag)
         (return-from read-xmlisp-end-tag t)
         (error "Tag mismatch: start tag=~A  end tag=~A" (xml-tag-name-symbol Self) End-Tag)))
      (t
       (error "Not a well formed end tag. Missing '>'")))))


(defmethod READ-XMLISP-ELEMENT ((Self xml-serializer) Stream)
  (let (($Sub-Element-Slot-Names$ nil))
    (declare (special $Sub-Element-Slot-Names$))
    ;; assume name has been read
    (read-white-space Stream)
    (read-xmlisp-attributes Self Stream)
    (finished-reading-attributes Self Stream)
    (loop
      (read-white-space Stream)
      (let ((Char (read-char Stream)))
        (case Char
          ;; end of element
          (#\/  
           (case (read-char Stream)
             (#\> ;; DONE!
              (return-from read-xmlisp-element Self))
             (t 
              (error "Not a well formed end tag. Missing '>'"))))
          ;; end tag
          (#\<  
           (case (read-char Stream)
             (#\/ 
              (when (read-xmlisp-end-tag Self Stream)
                (cleanup-sub-object-slots Self $Sub-Element-Slot-Names$)
                (return-from read-xmlisp-element Self)))
             (t 
              (error "Not a well formed end tag. Missing '/'"))))
          ;; content
          (#\>
           (loop 
             (read-white-space Stream)
             (let ((Next-Char (read-char Stream)))
               (case Next-Char
                 (#\<
                  (unless (read-xmlisp-element-content Self Stream)
                    (case (read-char Stream)
                      (#\/
                       (when (read-xmlisp-end-tag Self Stream)
                         (cleanup-sub-object-slots Self $Sub-Element-Slot-Names$)
                         (return-from read-xmlisp-element Self)))
                      (t (error "Not a well formed end tag. Missing '/'")))))
                 (t 
                  (unread-char Next-Char Stream)
                  (setf (content Self) (read-xmlisp-character-content Self Stream))))))))))))


(defmethod READ-XMLISP-ELEMENT :after ((Self xml-serializer) Stream)
  (finished-reading Self Stream))

;_____________________________
; File Input/Output           |
;_____________________________

(defun LOAD-OBJECT (Filename &key Verbose If-Does-Not-Exist)
  (when Verbose (format t ";; loading object in file: ~A~%" Filename))
  (with-open-file (File Filename :direction :input :if-does-not-exist If-Does-Not-Exist)
    (read File)))


(defmethod SAVE-OBJECT ((Self xml-serializer) Filename &key Verbose (If-Exists :error) (Xml-Header "<?xml version=\"1.0\"?>"))
  (when Verbose (format t ";; saving object to file: ~A~%" Filename))
  (with-open-file (File Filename :direction :output :if-exists If-Exists)
    (when XML-Header (format File "~A~%" XML-Header))
    (princ Self File)))

;_____________________________
; Set & Aggregation Handlers  |
;_____________________________


(defvar *Plural-Name-Table* (make-hash-table :test #'eq) "cached plural forms of symbols, e.g., bla -> blas")

(defun PLURAL-NAME (Name) "
  in: name symbol.
  out: plural-form-of-name symbol.
  Return plural form of <Name>"
  (or (gethash Name *Plural-Name-Table*)
      (setf (gethash Name *Plural-Name-Table*) (make-xmlisp-symbol (format nil "~As" Name)))))


(defmethod SET-ATTRIBUTE-VALUE ((Self xml-serializer) Name Value)
  ;; (format t "~%set attribute: ~A to: ~A" Name Value)
  ;; use MOP to find suitable slot with matching symbol-name
  (let* ((Slot-Definition (or (find-slot-definition Self Name)
                              (error "class: ~A does not have slot matching attribute name: ~A" (type-of Self) Name)))
         (Type (slot-definition-type Slot-Definition)))
    
    (setf (slot-value Self (slot-definition-name slot-definition))
          (if (eq Type t)
            ;; super generic type: need to explore other aspects of slot-definition
            (cond
             ;; try to infer from type of :initform
             ((numberp (slot-definition-initform Slot-Definition))
              (read-from-string Value))
             ;; no clues: fill in as string
             (t
              Value))
            ;; dispatch based on type
            (read-typed-attribute-value Value Type)))))


(defmethod ADD-OBJECT-TO-SLOT ((Self xml-serializer) Object Slot-Name)
  (declare (special $Sub-Element-Slot-Names$))
  ;; not very clever: needs to be reversed in cleanup
  (when (boundp '$Sub-Element-Slot-Names$)
    (pushnew Slot-Name $Sub-Element-Slot-Names$))  ;; keep track of slots for cleanup
  (push Object (slot-value Self Slot-Name)))


(defmethod CLEANUP-SUB-OBJECT-SLOTS ((Self xml-serializer) Slot-Names)
  ;; reverse lists to preserve same order as in stream
  (dolist (Slot-Name Slot-Names)
    (setf (slot-value Self Slot-Name) (reverse (slot-value Self Slot-Name)))))


(defmethod ADD-SUBOBJECT ((Self xml-serializer) Object)
  (let ((Name (xml-tag-name-symbol Object)))
    (let ((Single-Value-Slot-Definition (find-slot-definition Self Name)))
      (if Single-Value-slot-definition
        (setf (slot-value Self (slot-definition-name Single-Value-slot-definition)) Object)
        (let ((Multy-Value-Slot-Definition (find-slot-definition Self (plural-name Name))))
          (if Multy-Value-slot-definition
            (add-object-to-slot Self Object (slot-definition-name Multy-Value-slot-definition))
            (error "element: ~A of class: ~A does not have slots (\"~A\" or \"~A\") to contain sub element: ~A of class: ~A"
                   (xml-tag-name-symbol Self)
                   (type-of Self)
                   Name
                   (plural-name Name)
                   Name
                   (type-of Object))))))))

;_____________________________
; Print                       |
;_____________________________

(defun NUMBER-OF-PRINTABLE-ELEMENTS (Object) "
  Retun the number of object components that can be printed as XML elements."
   (let ((Number 0))
     (map-object 
      Object
      #'(lambda (Element) 
           (when (xml-printable-as-subelement-p Element)
             (incf Number))))
     Number))

(defvar *XML-Tab-Level* 0 "level of indentation")


(defun PRINT-XML-INDENT (Stream &optional (Level *XML-Tab-Level*))
  (dotimes (I Level)
    (princ "  " Stream)))


(defmethod PRINT-SLOT-VALUE-AS-ATTRIBUTE ((Self xml-serializer) Slot-Definition Stream)
   (let ((Type (slot-definition-type slot-definition))
         (Value (slot-value Self (slot-definition-name slot-definition))))
     (format Stream " ~A=" (string-downcase (symbol-name (slot-definition-name slot-definition))))
     (if Type
       ;; typed
       (print-typed-attribute-value Value Type Stream)
       ;; untyped
       (format 
        Stream
        "\"~A\""
        (etypecase Value
          (string (encode-xml-string Value))
          (number Value)
          (symbol Value)
          (list Value))))))


(defmethod PRINT-SLOTS-AS-ATTRIBUTES ((Self xml-serializer) Slot-Definitions Stream)
  (dolist (Slot-Definition slot-definitions)
    (let ((Value (slot-value Self (slot-definition-name slot-definition))))
      ;; make sure there is a meaninful way to print the value
      (when (or *XMLisp-Output-Non-Default-Value-Attributes* (not (equal Value (slot-definition-initform slot-definition))))
        (print-slot-value-as-attribute Self slot-definition Stream)))))


(defmethod PRINT-SLOT-VALUE-AS-SUBELEMENT ((Self xml-serializer) Slot-Definition Stream)
  (let ((Type (slot-definition-type slot-definition))
        (Value (slot-value Self (slot-definition-name slot-definition))))
    (if Type
      ;; Typed
      (print-typed-subelement-value Value Type Stream)
      ;; untyped
      (map-object 
       Value
       #'(lambda (Object)
           (terpri Stream)
           (print-object Object Stream))))))


(defmethod PRINT-SLOTS-AS-SUBELEMENTS ((Self xml-serializer) Slot-Definitions Stream)
  (dolist (Slot-Definition slot-definitions)
    ;; (format t "~%print slot: ~A" (slot-definition-name slot-definition))
    (print-slot-value-as-subelement Self slot-definition Stream)))


(defmethod SLOTS-TO-PRINT-LIST ((Self xml-serializer))
  (let ((Slot-Names (print-slots Self)))
    (if (equal Slot-Names :all)
      (class-slots (class-of Self))
      (mapcar 
       #'(lambda (Slot-Name) 
           (or (find-slot-definition Self Slot-Name)
               (error "print error: Class \"~A\" does not have slot \"~A\"" (type-of Self) Slot-Name)))
       Slot-Names))))


(defmethod PRINT-OBJECT ((Self xml-serializer) Stream)
  ;; start tag
  (print-xml-indent Stream)
  (format Stream "<~A" (xml-tag-name-string Self))
  ;; separate printable subelements from others
  (let ((Attribute-Value-Printable-Slot-Definitions nil)
        (Subelement-Printable-Slot-Definitions nil))
    (dolist (Slot-Definition (reverse (slots-to-print-list Self)))
      (when (print-slot-with-name-p Self (slot-definition-name slot-definition))
        (let ((Value (slot-value Self (slot-definition-name slot-definition))))
          (cond
           ((xml-printable-as-subelement-p Value)
            (push slot-definition Subelement-Printable-slot-definitions))
           ((xml-printable-as-attribute-value-p Value)
            (push slot-definition Attribute-Value-Printable-slot-definitions))
           (t
            (warn "\"~A\" stored in slot ~A is not XML printable" Value (slot-definition-name slot-definition)))))))
    ;; print single <.../> or nested one
    (cond
     ;; at least one sub element or some content
     ((and (print-subelements-to-stream-p Self Stream) (or Subelement-Printable-slot-definitions (content Self)))
      ;; start tag
      (print-slots-as-attributes Self Attribute-Value-Printable-slot-definitions Stream)
      (format Stream ">")
      ;; content
      (when (content Self) (princ (encode-xml-string (content Self)) Stream))
      ;; sub elements
      (let ((*Xml-Tab-Level* (1+ *XML-Tab-Level*)))
        (print-slots-as-subelements Self Subelement-Printable-slot-definitions Stream))
      ;; end tag
      (unless (content Self)
        (terpri Stream)
        (print-xml-indent Stream))
      (format Stream "</~A>" (xml-tag-name-string Self)))
     ;; simple tag: no sub elements, no content
     (t
      (print-slots-as-attributes Self Attribute-Value-Printable-slot-definitions Stream)
      (format Stream "/>")))))

;_____________________________
; Read                        |
;_____________________________

(defun ELEMENT-READER (Stream Char) 
  (declare (ignore Char))
  ;; may not be an XML element after all, e.g., common-lisp functions <, <=
  (let ((Next-Char (read-char Stream nil nil)))
    ;; dange zone: may not catch all the cases. 
    ;; Probably better approach: if next-char is not a valid first character of a XML element name then 
    ;; finish reading the symbol and return it
    (case Next-Char
      (#\space (return-from element-reader (intern "<")))
      (#\= (return-from element-reader (intern "<="))))
    (unread-char Next-Char Stream))
  ;; lets read XML
  (skip-xml-header Stream)   ;; this only needs to be done once
  (read-xmlisp-element (read-name-and-make-instance Stream) Stream))


;;; FIXME: this does not nest properly. I think it's better to provide
;;; a readtable for the user to enable & disable.
(let ((read-syntax-enabled)
      (<-macro-character nil))
  (defun enable-xmlisp-read-syntax ()
    (unless read-syntax-enabled
      (setf read-syntax-enabled t)
      (setf <-macro-character (multiple-value-list (get-macro-character #\<)))
      (set-macro-character #\< #'element-reader t)))
  (defun disable-xmlisp-read-syntax ()
    (when read-syntax-enabled
      (setf read-syntax-enabled nil)
      (apply #'set-macro-character #\< <-macro-character))))

(enable-xmlisp-read-syntax)

;_____________________________
; Platform Specific Printing   |
;_____________________________

#+(and :mcl (not :openmcl))
(defmethod PRINT-SUBELEMENTS-TO-STREAM-P ((Self xml-serializer) (Stream inspector::cache-entry-stream))
  ;; never print sublelements in inspector
  (declare (ignore Stream))
  nil)


#+(and :mcl (not :openmcl))
(defmethod PRINT-SLOT-VALUE-AS-SUBELEMENT ((Self xml-serializer) Slot-Definition (Stream ccl::terminal-io))
   (let ((Value (slot-value Self (slot-definition-name slot-definition))))
     ;; in Listener only print all subelements if there are not that many
     (cond
      ((<= (number-of-printable-elements Value) 100)
       (call-next-method))
      ;; too many!!
      (t
       (terpri Stream)
       (print-xml-indent Stream)
       (format Stream "... with ~A ~A subelements ..." (number-of-printable-elements Value) (slot-definition-name slot-definition))))))


#| 

;; Example 1: HTML Link
;;   simple mapping between class/element and slot/attribute name


(defclass A (xml-serializer)
   ((href :accessor href :initform "" :initarg :href))
   (:documentation "HTML link"))


(inspect <a href="http://www.agentsheets.com"/>)
<a href="http://www.agentsheets.com">AgentSheets</a>


;; Example 2: RSS

(defclass RSS (xml-serializer)
   ((version :accessor version :initform "")
    (channel :accessor channel :initform nil))
   (:documentation "RSS main element"))

(defclass CHANNEL (xml-serializer)
   ((title :accessor title :initform "")
    (link :accessor link :initform "")
    (description :accessor description :initform "")
    (image :accessor image :initform nil)
    (managingeditor :accessor managingeditor :initform "")
    (ttl :accessor ttl :initform nil :documentation "don't know what this is")
    (language :accessor language :initform "")
    (copyright :accessor copyright :initform "")
    (webmaster :accessor webMaster :initform "")
    (pubdate :accessor pubDate :initform "")
    (lastbuilddate :accessor lastBuildDate :initform "")
    (category :accessor category :initform "")
    (generator :accessor generator :initform "")
    (docs :accessor docs :initform "")
    (items :accessor items :initform nil :documentation "list of RSS item"))
   (:documentation "RSS channel"))


(defclass IMAGE (xml-serializer)
   ((title :accessor title :initform "")
    (url :accessor url :initform "")
    (link :accessor link :initform "")
    (width :accessor width :initform 0))
   (:documentation "RSS Image"))


(defclass ITEM (xml-serializer)
   ((title :accessor title :initform "")
    (link :accessor link :initform "")
    (description :accessor description :initform "")
    (pubdate :accessor pubdate :initform ""))
   (:documentation "RSS news Item"))

;; pick an XML RSS file from the examples/xml folder
;; if you pick other RSS files keep in mind that the above spec is incomplete


(defparameter *RSS-News* (load-object (ccl:choose-file-dialog)))

(save-object *RSS-News* "ccl:delete_me.xml" :if-exists :overwrite)


;; and walk throught the RSS structure

(inspect *RSS-News*)



;; Example 3: Typed Slots
;;   Typed slots use the print-typed-attribute-value, read-typed-attribute-value, print-typed-subelement-value
;;   CODECs 

(defclass COIN (xml-serializer)
  ((head-is-up :accessor head-is-up :type boolean)))


(inspect <coin head-is-up="true"/>)



;; Example 4: simple Aggregation: rule based Visual AgenTalk-like language 
;;  use MOP name matching to implement aggregation
;;  e.g. slot "RULES" will contain a list of "RULE" elements

(defclass COMMAND (xml-serializer)
  ((name :accessor name :initform "" :initarg :name)
   (comments :accessor comments :initform nil)))


(defclass BEHAVIOR (command)
  ((method-commands :accessor method-commands :initform nil)))


(defclass METHOD-COMMAND (command)
  ((trigger :accessor trigger :initform nil)
   (rules :accessor rules :initform nil)))

(defclass TRIGGER (command)
  ())

(defclass RULE (command)
  ((condition-commands :accessor condition-commands :initform nil :initarg :condition-commands)
   (action-commands :accessor action-commands :initform nil :initarg :action-commands)
   (is-enabled :accessor is-enabled :initform t :initarg :is-enabled :type boolean)
   (probablility :accessor probability :initform 0.9s0 :initarg :probability :type short-float)))


(defclass CONDITION-COMMAND (command)
  ())


(defclass ACTION-COMMAND (command)
  ())


(defclass TRIGGER (command)
  ())



(inspect
 <behavior name="random Move">
   <method-command name="mouse" trigger="on mouse down">
     <rule>
       <condition-command name="see_a"/>
       <condition-command name="key"/>
       <action-command name="play_sound"/>
     </rule>
   </method-command>
 </behavior> )


;; Example 5: User defined Aggregation
;;   This is by no means a complete definition


(defclass HTML-BASE-CLASS (xml-serializer)
  ()
  (:documentation "mother of all HTML element classes"))


(defclass HTML (html-base-class)
  ((items :accessor items :initform nil))
  (:documentation "Contains all the html items of an HTML document"))


(defmethod ADD-SUBOBJECT ((Self html) (Item html-base-class))
  ;; extend this method to add all html-base-class instances to the "items" slot
  (add-object-to-slot Self Item 'items))
  


(defclass A (html-base-class)
   ((href :accessor href :initform "" :initarg :href))
   (:documentation "HTML link"))


(defclass FONT (html-base-class)
  ((face :accessor face)
   (size :accessor size :type number))
  (:documentation "Font info"))




(inspect

<HTML>

<font face="arial, sans-serif" size="-2">Small Text here</font>
<font face="arial, sans-serif" size="+2">Large Text here</font>
<a href="http://www.cs.colorado.edu">Go CU</a>

</HTML>   )


|#

