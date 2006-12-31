;;;-*- Mode: Lisp; Package:  xml -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L   P A R S E R                                *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2000, Agentsheets Inc.                       *
;* Filename  : xml-parser.lisp                                       *
;* Updated   : 04/19/01                                              *
;* Version   :                                                       *
;*    1.0    : 04/20/00 defined PARSE-XML-FILE                       *
;*    1.1    : 04/28/00 finished parsing of xml elements             *
;*    1.1.1  : 01/23/01 reimplemented PARSE-XML,added $Base-Element$ *
;*           :          used accessors to get xml-element definitions*
;*    1.2    : 04/19/01 calling element action                       *
;*    1.3    : 09/06/01 introduced *Xml-Relaxed-Case*                *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Parses XML files.                                     *
;*********************************************************************

(in-package :xml)

(defparameter *XML-RELAXED-CASE* nil "ignore uppercase letters in xml tags?")

(defun PARSE-XML-FILE (Filename)
  (with-open-file (File Filename :direction :input)
    (setq *Xml-File-Being-Parsed* Filename)
    (parse-xml File)))


(defun PARSE-XML (Stream)
  (declare (special $Base-Element$))
  (let ((Base-Xml-Element nil)
        (Char nil))
    (setq $Base-Element$ t)
    ;; read xml file header and if valid continue
    (when (read-prolog Stream)
      (format-if-verbose t "Finished reading prolog")
      ;; read xml content: base element
      (unless (catch :read-element-error
                (read-white-space Stream)
                (or (setq Char (read-char Stream nil nil)) (return-from PARSE-XML Base-Xml-Element))
                (when (char= Char #\<)
                  (setq Base-Xml-Element (make-instance 'xml-element))
                  (unless (read-element Base-Xml-Element Stream)
                    (format-if-verbose t "Found an invalid element"))))
        (format-if-verbose t "finished reading XML file")
        Base-Xml-Element))))


;; will have to fix! => add READ-XML-DECL
(defun READ-PROLOG (Stream)
  #|(let ((XML-Decl (or (read Stream) (throw :read-prolog-error nil)))
        (End-Decl nil)
        (Version-Info nil)
        (Encoding-Symbol nil)
        (Encoding nil))
    (cond ((string-equal (symbol-name XML-Decl) "<?XML")
           (or (read Stream) (throw :read-prolog-error nil))
           (setq Version-Info (or (read Stream) (throw :read-prolog-error nil)))
           (cond ((string= Version-Info *XML-File-Version*)
                  (setq Encoding-Symbol (or (read Stream) (throw :read-prolog-error nil)))
                  (when (string=  (symbol-name Encoding-Symbol) "ENCODING=")
                    (setq Encoding (or (read Stream) (throw :read-prolog-error nil))))
                  (read-misc Stream)
                  (read-document-type-decl Stream)
                  (setq End-Decl (or (read Stream) (throw :read-prolog-error nil)))
                  (if (string-equal (symbol-name End-Decl) "?>")
                    (return-from READ-PROLOG t)
                    (format-if-verbose t "Not a valid XML file prolog: missing '?>'")))
                 (t (format-if-verbose t "Not a valid XML file prolog: unsupported version number"))))
          (t (format-if-verbose t "Not a valid XML file prolog: invalid XML definition"))))|#
  ;; instead read from start tag to end tag.. ignore content FOR NOW!!
  (let ((Char nil))
    (loop
      (or (setq Char (read-char Stream nil nil)) (throw :read-prolog-error nil))
      (when (char= Char #\>)
        (return-from READ-PROLOG t))))) 
           

;; will have to really implement!
(defun READ-MISC (Stream)
  (read-white-space Stream))

;; will have to really implement!
(defun READ-DOCUMENT-TYPE-DECL (Stream)
  (read-white-space Stream))


(defmethod READ-ELEMENT ((Self Xml-Element) Stream)
  (declare (special $Base-Element$))
  (let ((Char nil)
        (Next-Char nil)
        (Start-Tag nil)
        (Xml-Element-Definition nil))
    (loop
      (read-white-space Stream)
      (or (setq Char (read-char Stream nil nil)) (throw :read-element-error nil))
      (cond 
       ;; end of element
       ((char= Char #\/)
        (or (setq Next-Char (read-char Stream nil nil)) (throw :read-element-error nil))
        (cond ((char= Next-Char #\>)
               (setq Char (read-char Stream nil nil))
               ;; launch end-tag actions
               (mapcar #'(lambda (End-Tag-Action)
                           (funcall End-Tag-Action Self))
                       (end-tag-actions Xml-Element-Definition))
               (unread-char Char Stream)
               (return-from READ-ELEMENT Self))
              (t 
               (format-if-verbose t "Not a well formed end tag. Missing '>'")
               (throw :read-element-error nil))))
       ;; end tag
       ((char= Char #\<)
        (or (setq Next-Char (read-char Stream nil nil)) (throw :read-element-error nil))
        (cond ((char= Next-Char #\/)
               (when (read-end-tag Stream Start-Tag)
                 ;; launch end-tag actions
                 (mapcar #'(lambda (End-Tag-Action)
                             (funcall End-Tag-Action Self))
                         (end-tag-actions Xml-Element-Definition))
                 (return-from READ-ELEMENT Self)))
              (t 
               (format-if-verbose t "Not a well formed end tag. Missing '/'")
               (throw :read-element-error nil))))
       ;; content
       ((char= Char #\>)
        (loop 
          (read-white-space Stream)
          (or (setq Next-Char (read-char Stream nil nil)) (throw :read-element-error nil))
          (cond ((char= Next-Char #\<)
                 (unless (read-element-content Self Stream)
                   (or (setq Next-Char (read-char Stream nil nil)) (throw :read-element-error nil))
                   (cond ((char= Next-Char #\/)
                          (when (read-end-tag Stream Start-Tag)
                            ;; launch end-tag actions
                            (mapcar #'(lambda (End-Tag-Action)
                                        (funcall End-Tag-Action Self))
                                    (end-tag-actions Xml-Element-Definition))
                            (return-from READ-ELEMENT Self)))
                         (t (format-if-verbose t "Not a well formed end tag. Missing '/'")
                            (throw :read-element-error nil)))))
                (t 
                 (unread-char Next-Char Stream)
                 (setf (content Self) (read-character-content Self Stream))))))
       ;; start tag
       (t
        (unread-char Char Stream)
        (setq Start-Tag (intern (if *Xml-Relaxed-Case* 
                                  (string-downcase (read-name Stream))
                                  (read-name Stream))
                                :xml))
        ;; what am i doing here?
        (when $Base-Element$ 
          (setq *Xml-Document* Start-Tag)
          (setq $Base-Element$ nil))
        (setq Xml-Element-Definition (get-xml-element-definition Start-Tag))
        (cond (Xml-Element-Definition
               (setf (name Self) Start-Tag)
               ;; launch start-tag actions
               (mapcar #'(lambda (Start-Tag-Action)
                           (funcall Start-Tag-Action Self))
                       (start-tag-actions Xml-Element-Definition)))
              (t 
               (format-if-verbose t "No such XML element")
               (throw :read-element-error nil)))
        (read-white-space Stream)
        (read-attributes Self Stream))))))




(defmethod READ-END-TAG (Stream Start-Tag)
  (let ((End-Tag (intern (if *Xml-Relaxed-Case*
                           (string-downcase (read-name Stream))
                           (read-name Stream)) :xml))
        (Char nil))
    (read-white-space Stream)
    (or (setq Char (read-char Stream nil nil)) (throw :read-element-error nil)) 
    (cond ((char= Char #\>) 
           ;; match tags
           (cond ((eq Start-Tag End-Tag)
                  (return-from READ-END-TAG t))
                 (t 
                  (format-if-verbose t "Tag mismatch: start tag=~A  end tag=~A" Start-Tag End-Tag)
                  (throw :read-element-error nil))))
          (t
           (format-if-verbose t "Not a well formed end tag. Missing '>'")
           (throw :read-element-error nil)))))



(defmethod READ-ATTRIBUTES ((Self Xml-Element) Stream)
  (let ((Char nil)
        (Attribute-Name nil)
        (Attribute-Value nil))
    (loop
      (or (setq Char (read-char Stream nil nil)) (throw :read-element-error nil))
      (cond ((or (char= Char #\/) (char= Char #\>))
             (unread-char Char Stream)
             (return t))
            (t
             (unread-char Char Stream)
             (setq Attribute-Name (intern (string-upcase (read-name Stream)) :xml))
             (read-equal-sign Stream)
             (setq Attribute-Value (read-value Stream))
             (read-white-space Stream)
             (if (member Attribute-Name (element-attributes (get-xml-element-definition (name Self))))
               (set-element-attribute Self Attribute-Name Attribute-Value)
               (progn (format-if-verbose t "'~A' is not a valid attribute for element '~A'" Attribute-Name (name Self))
                      (throw :read-element-error nil))))))))



(defmethod READ-ELEMENT-CONTENT ((Self Xml-Element) Stream)
  (let ((Char (or (read-char Stream nil nil) (throw :read-element-error nil)))
        (Element (make-instance 'xml-element :container Self)))
    (cond 
     ;; found an empty element or the end of this element
     ((char= Char #\/)
      (unread-char Char Stream)
      (return-from READ-ELEMENT-CONTENT nil))
     (t
      (unread-char Char Stream)
      (read-element Element Stream)
      (if (content Self)
        (nconc (content Self) (list Element))
        (setf (content Self) (append (content Self) (list Element))))))))


(defmethod READ-CHARACTER-CONTENT ((Self Xml-Element) Stream)
  (let ((Char nil))
    (with-output-to-string (Content)
      (loop
        (setq Char (or (read-char Stream nil nil) (throw :read-element-error nil)))
        (cond 
         ;; found the end of character content
         ((char= Char #\<)
          (unread-char Char Stream)
          (return))
         (t
          (princ Char Content)))))))


(defun READ-NAME (Stream) "
  Valid names start with a letter, _ or :, have to contain letters or digits or other valid characters (see XML spec)"
  (let ((Char (or (read-char Stream nil nil) (throw :read-element-error nil))))
    ;; check first character
    (cond ((or (letterp Char)
               (char= Char #\_)
               (char= Char #\:))
           ;; read and check the rest of the characters: stop if you find white space, = or >
           (with-output-to-string (Name)
             (princ Char Name)
             (loop
               (setq Char (or (read-char Stream nil nil) (throw :read-element-error nil)))
               (cond ((or (white-space-p Char)
                          (char= Char #\=)
                          (char= Char #\>)
                          (char= Char #\/))
                      (unread-char Char Stream)
                      (return Name))
                     ((namecharp Char)
                      (princ Char Name))
                     (t (format-if-verbose t "Character ~C is not a valid character for a name" Char)
                        (throw :read-element-error nil))))))
          (t (format-if-verbose t "Not a valid start character for name")
             (throw :read-element-error nil)))))


(defun READ-WHITE-SPACE (Stream)
  (let ((Char nil))
    (loop 
      (or (setq Char (read-char Stream nil nil)) (throw :read-element-error nil))
      (unless (white-space-p Char)
        (unread-char Char Stream)
        (return t)))))


(defun READ-VALUE (Stream)
  (let ((Char (or (read-char Stream nil nil) (throw :read-element-error nil))))
    (if (char= Char #\")
      (with-output-to-string (Value)
        (loop
          (setq Char (or (read-char Stream nil nil) (throw :read-element-error nil)))
          (if (char= Char #\")
            (return Value)
            (princ Char Value)))))))



(defun READ-EQUAL-SIGN (Stream)
  (read-white-space Stream)
  (let ((Char (or (read-char Stream nil nil) (throw :read-element-error nil))))
    (if (char= Char #\=)
      (read-white-space Stream)
      (format-if-verbose t "Did not find an equal sign"))))



#| Example:

(setq *print-verbose* t)
(defvar *xml-elements* nil)
(setq *xml-elements* (parse-xml-file (ccl::choose-file-dialog)))
(time (setq *xml-elements* (parse-xml-file #P"Macintosh HD:Desktop Folder:test.xml")))

|#