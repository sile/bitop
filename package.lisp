(defpackage :bitop    ; TODO: package.lisp
  (:use :common-lisp)
  (:export make-fixbyte
	   fixbyte
	   fixbyte-fixnum
	   fixbyte-length
	   fixbyte-reverse))
(in-package :bitop)

(defparameter *optimize* '(optimize (speed 3) (safety 0) (debug 0) (compilation-speed 0)))

(deftype fixnum-length () '(integer 0 #.(integer-length most-positive-fixnum)))
