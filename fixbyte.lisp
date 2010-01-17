(in-package :bitop)

(declaim #.*optimize*
	 (inline fixbyte))

(defmacro defconst-once-only (name value &optional documentation)
  `(unless (boundp ',name)
     (defconstant ,name ,value ,documentation)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (fixbyte (:constructor make-fixbyte (fixnum length)))
    (fixnum 0 :type fixnum        :read-only t)
    (length 0 :type fixnum-length :read-only t)))

(defmethod print-object ((o fixbyte) stream)
  (print-unreadable-object (o stream)
    (format (the stream stream) "~V,'0B" (fixbyte-length o) (fixbyte-fixnum o))))

(defmethod make-load-form ((o fixbyte) &optional env)
  (declare (ignore env))
  (make-load-form-saving-slots o))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +CACHED-BIT-LENGTH-LIMIT+ 15)
  (defun generate-fixbyte-table (&aux (size (1+ +CACHED-BIT-LENGTH-LIMIT+)))
    #+SBCL (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
    (make-array size
      :element-type '(simple-array fixbyte)
      :initial-contents
      (loop FOR len FROM 0 BELOW size COLLECT
        (make-array (expt 2 len) 
          :element-type 'fixbyte
          :initial-contents
	  (loop FOR num FROM 0 BELOW (expt 2 len) COLLECT
	    (make-fixbyte num len))))))

  (defun fixbyte-reverse (fb &aux (r 0))
    (declare (fixnum r)
	     (fixbyte fb))
    (with-slots (fixnum length) fb
      (dotimes (i length (make-fixbyte r length))
	(setf r (+ (ash r 1) (ldb (byte 1 i) fixnum))))))

  (defconst-once-only +FIXBYTE-TABLE+ (generate-fixbyte-table))
  (defconst-once-only +REVBYTE-TABLE+
                        (map 'vector
			     (lambda (fixbyte-ary)
			       (map 'vector #'fixbyte-reverse fixbyte-ary))
			     +FIXBYTE-TABLE+)))

(defun fixbyte (fixnum length &key reverse)
  (declare (fixnum fixnum) (fixnum-length length))
  (macrolet ((table-ref (name num)
               `(aref (the (simple-array fixbyte) (aref ,name length)) ,num)))
    (let ((fixnum (ldb (byte length 0) fixnum)))
      (if reverse
	  (if (<= length +CACHED-BIT-LENGTH-LIMIT+)
	      (table-ref +REVBYTE-TABLE+ fixnum)
	    (fixbyte-reverse (make-fixbyte fixnum length)))
	(if (<= length +CACHED-BIT-LENGTH-LIMIT+)
	    (table-ref +FIXBYTE-TABLE+ fixnum)
	  (make-fixbyte fixnum length))))))