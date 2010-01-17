(in-package :bitop)

(declaim #.*optimize*
	 (inline write-fixbyte buffered-write-byte))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +STREAM-BUFFER-SIZE+ 4096))

(defstruct (bit-stream (:constructor make-bit-stream (stream)))
  (buf (make-array #1=#.+STREAM-BUFFER-SIZE+ :element-type 'octet) :type (simple-octets #1#))
  (i     0 :type (integer 0 #1#))

  stream
  (octet 0 :type octet)
  (pos   0 :type (integer 0 8)))

;;;;;;;;;;;;;
;;;; function
(defun buffered-write-byte (byte bit-stream)
  (declare (octet byte) (bit-stream bit-stream))
  (with-slots (buf i stream) bit-stream
    (when (= i +STREAM-BUFFER-SIZE+)
      (write-sequence buf stream)
      (setf i 0))
    (setf (aref buf i) byte)
    (incf i)))

(defmacro with-bit-stream ((stream source-binary-stream) &body body)
  `(let ((,stream (make-bit-stream ,source-binary-stream)))
     (unwind-protect
         (progn ,@body)
       (when (output-stream-p (bit-stream-stream ,stream))
         (flush-bit-stream ,stream)))))

(defun write-fixbyte (fixbyte bit-stream)
  (declare (bit-stream bit-stream) 
	   (fixbyte fixbyte))
  (with-slots (octet pos) bit-stream
    (let ((num  (fixbyte-fixnum fixbyte))
	  (len  (fixbyte-length fixbyte))
	  (room (- 8 pos)))
      (setf octet (ldb (byte 8 0) (+ octet (ash num pos))))

      (when (< (+ pos len) 8)
	(incf pos len)
	(return-from write-fixbyte))

      (buffered-write-byte octet bit-stream)
      (do ((len (- len   room)     (- len 8))
	   (num (ash num (- room)) (ash num -8)))
	  ((< len 8)  
	   (setf pos   len
		 octet (ldb (byte 8 0) num)))
        (declare (fixnum num))
	(buffered-write-byte (ldb (byte 8 0) num) bit-stream)))))

(defun flush-bit-stream (bit-stream)
  (declare (bit-stream bit-stream))
  (with-slots (stream octet pos buf i) bit-stream
    (unless (zerop pos)
      (buffered-write-byte octet bit-stream))
    (write-sequence buf stream :end i)))

(defun read-bit (bit-stream)
  (declare (bit-stream bit-stream))
  (with-slots (stream octet pos) bit-stream
    (when (= 0 pos)
      (setf octet (read-byte stream))
      (setf pos 8))
    (prog1 
        (ldb (byte 1 (- 8 pos)) octet)
      (decf pos))))