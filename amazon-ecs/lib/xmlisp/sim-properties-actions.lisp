;;;-*- Mode: Lisp; Package:  ccl -*-
;*********************************************************************
;*                                                                   *
;*    PROGRAM     X M L     A C T I O N S                            *
;*                                                                   *
;*********************************************************************
;* Author    : Andri Ioannidou (andri@agentsheets.com)               *
;*             http://www.agentsheets.com                            *
;* Copyright : (c) 1996-2001, Agentsheets Inc.                       *
;* Filename  : xml-actions.lisp                                      *
;* Updated   : 02/12/02                                              *
;* Version   :                                                       *
;*    1.0    : 08/28/01 implemented sim-properties element actions   *
;*    1.1    : 08/31/01 implemented properties and interface actions *
;*    1.1.1  : 02/08/02 disabled Define Interface for properties     *
;*    1.2    : 02/11/02 plotter not part of interface anymore        *
;*    1.3    : 02/12/02 factored out common code for creating editor *
;* HW/SW     : PowerPC G4, MCL 4.2 & 3.3                             *
;* Abstract  : Implements xml-element actions performed before and   *
;*             after reading the elements                            *
;*********************************************************************

(in-package :ccl) 

;__________________________________________
;  S I M   P R O P E R T I E S   actions  /
;________________________________________

(export '(CREATE-PROPERTY-EDITOR
         CREATE-EMPTY-PROPERTY-EDITOR
         PROPERTY-INITIALIZATION
         ADD-PROPERTY-TO-EDITOR
         FINISH-PROPERTY-EDITOR-SETUP
         GET-INTERFACE-INFO
           GET-PLOTTER-INFO))


(defparameter *Interface* nil)
(defparameter *Plotter* nil)

(defun CREATE-PROPERTY-EDITOR (Simproperties-Element)
  (declare (ignore Simproperties-Element))
  (create-empty-property-editor)
  (xml:format-if-verbose t "creating property editor window: ~A" *Property-Editor*))


(defun CREATE-EMPTY-PROPERTY-EDITOR ()
  (let ((Labeled-List 
         (make-instance 
           'labeled-scrollable-command-list 
           :title-labels '("Property Name" "Property Value ")))
        (Buttons `(("New" ,#'New-Property "Creates a new Simulation Property")
                   ("Edit" ,#'Edit-Property "Edits the name of the currently selected Simulation Property")
                   ("Plot" ,#'Plot-Property "Defines plot parameters for the currently selected Simulation Property"))))
    (setq *Property-Editor*
          (make-instance 'property-editor-window
            :allow-other-keys t
            :window-title *Property-Editor-Default-Name*
            :filename *Property-Editor-Filename*
            :window-show nil
            :command Labeled-List
            :buttons Buttons))))


(defun FINISH-PROPERTY-EDITOR-SETUP (Simproperties-Element)
  ;; check version ??
  (let ((X (read-from-string (xml:get-element-attribute Simproperties-Element 'xml::x)))
        (Y (read-from-string (xml:get-element-attribute Simproperties-Element 'xml::y)))
        (Width (read-from-string (xml:get-element-attribute Simproperties-Element 'xml::width)))
        (Height (read-from-string (xml:get-element-attribute Simproperties-Element 'xml::height))))
    (resize-outside-in *Property-Editor* (make-point Width Height))
    (set-view-position *Property-Editor* X Y)
    (setf (needs-saving *Property-Editor*) nil)
    (window-ensure-on-screen *Property-Editor*)
    (window-show *Property-Editor*))
  (xml:format-if-verbose t "Finished creating editor window: ~A" *Property-Editor*))


(defun PROPERTY-INITIALIZATION (Property-Element)
  (declare (ignore Property-Element))
  (setq *Interface* nil)
  (setq *Plotter* nil))


(defun ADD-PROPERTY-TO-EDITOR (Property-Element)
  (let ((Name (xml:get-element-attribute Property-Element 'xml::name))
        (Value (read-from-string (xml:get-element-attribute Property-Element 'xml::value)))
        (Input-P (if (string-equal (xml:get-element-attribute Property-Element 'xml::input) "true") t nil))
        (Output-P (if (string-equal (xml:get-element-attribute Property-Element 'xml::output) "true") t nil))
        (Labeled-List (aref (view-subviews *Property-Editor*) 0)))
    (xml:format-if-verbose t "Adding the ~A property to the editor.." Name)
    (xml:format-if-verbose t "Interface spec: ~A" *Interface*)
    (create-and-add-property (list Name Value (list Input-P Output-P) *Interface* *Plotter*) Labeled-List)))


(defun GET-INTERFACE-INFO (Interface-Element)
  (let* ((Interface-Spec-Element (first (xml:content Interface-Element)))  ;; only one interface spec exists
         (Type (xml:name Interface-Spec-Element)))
    (xml:format-if-verbose t "Type of Interface: ~S" Type)
    (xml:format-if-verbose t "xml comparison: ~A" (equal Type 'xml::|slider|))
    (cond ((equal Type 'xml::|slider|)
           (xml:format-if-verbose t "Slider Interface")
           (let ((Min (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::min)))
                 (Max (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::max))))
             (setq *Interface* `(:type Slider :min ,Min :max ,Max))))
          #|((equal Type 'xml::|plotter|)
           (xml:format-if-verbose t "Plotter Interface")
           (let ((Min (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::min)))
                 (Max (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::max)))
                 (Timestep (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::timestep)))
                 (Foreground (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::foreground)))
                 (Background (read-from-string (xml:get-element-attribute Interface-Spec-Element 'xml::background))))
             (setq *Interface* `(:type Plotter :min ,Min :max ,Max :timestep ,Timestep :foreground ,Foreground :background ,Background))))|#
          (t (xml:format-if-verbose t "no interface") nil))))



(defun GET-PLOTTER-INFO (Plotter-Element)
  (let ((Enabled (if (string-equal (xml:get-element-attribute Plotter-Element 'xml::enabled) "true") t nil))
        (Min (read-from-string (xml:get-element-attribute Plotter-Element 'xml::min)))
        (Max (read-from-string (xml:get-element-attribute Plotter-Element 'xml::max)))
        (Window (xml:get-element-attribute Plotter-Element 'xml::window))
        (Representing (xml:get-element-attribute Plotter-Element 'xml::representing))
        (Color (read-from-string (xml:get-element-attribute Plotter-Element 'xml::color))))
    (setq *Plotter* `(:enabled ,Enabled :min ,Min :max ,Max :window ,Window :representing ,Representing :color ,Color))))
         
