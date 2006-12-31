;;;-*- Mode: Lisp; Package: COMMON-LISP-USER -*-
;;; XMLisp loader  
;;; Use this if you do not want to use the Defsystem way of loading

(in-package :cl-user)

(defpackage XML
  (:use "COMMON-LISP"))


; this is just a simpler loader. Compile the files to load faster

(defun OPERATE-RELATIVE (Filename &optional (Operation #'load))
  (funcall 
   Operation
   (make-pathname
    :directory (pathname-directory 
                #+:mcl *Loading-File-Source-File* ;; works also with MCL fred eval
                #-:mcl *Load-Truename*)           ;; CL version
    :name Filename)))


(defvar *XMLisp-Files*
  '("xml-utilities"
    "element-definition"
    "xml-tag-definitions"
    "element-implementation"
    "name-character-check"
    "xml-parser"
    "xml-writer"
    "XMLisp")
  "all the XMLisp files")


(defun LOAD-XMLISP ()
  (dolist (File *XMLisp-Files*)
    (format t ";;; loading ~S~%" File)
    (operate-relative File)))

(load-xmlisp)

;; Now go to the XMLisp.lisp files and play with the examples at the bottom.

#-:openmcl (operate-relative "XMLisp.lisp" #'ed)
    
