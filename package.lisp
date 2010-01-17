(defpackage :bitop    ; TODO: package.lisp
  (:use :common-lisp)
  (:export make-fixbyte
	   fixbyte
	   fixbyte-fixnum
	   fixbyte-length
	   fixbyte-reverse

	   bit-stream
           make-bit-stream
           flush-bit-stream
           with-bit-stream
           write-fixbyte
           read-bit))
(in-package :bitop)

(defparameter *optimize* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(deftype fixnum-length () '(integer 0 #.(integer-length most-positive-fixnum)))
(deftype octet () '(unsigned-byte 8))
(deftype simple-octets (size) `(simple-array (unsigned-byte 8) (,size)))
