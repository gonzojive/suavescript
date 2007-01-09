(defpackage :red-util
  (:use :cl)
  (:export :slurp-stream4 :slurp-stream :slurp-binary-stream))
(in-package :red-util)
; from http://www.emmett.ca/~sabetts/slurp.html

(defun slurp-stream4 (stream)
  (let ((seq (make-string (file-length stream))))
    (read-sequence seq stream)
    seq))

(defun slurp-stream (stream) (slurp-stream4 stream))

(defun slurp-binary-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type '(unsigned-byte 8))))
    (read-sequence seq stream)
    seq))