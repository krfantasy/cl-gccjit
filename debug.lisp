(in-package :cl-gccjit)

(defvar *debug-logging* nil
  "When T, logs all FFI function calls")

(defvar *debug-logging-destination* t
  "Destination for debug logging output that used by `format', defaults to T")

(defun format-ffi-arg (type value)
  (case type
    (:pointer (if (cffi:null-pointer-p value) "NULL" (format nil "#<pointer 0x~x>" value)))
    ((:short :unsigned-short :int :unsigned-int :long :unsigned-long :long-long :unsigned-long-long
      :int8 :uint8 :int16 :uint16 :int32 :uint32 :int64 :uint64 :size :ssize :intptr :uintptr :ptrdiff :offset)
     (format nil "~d" value))
    ((:float :double) (format nil "~f" value))
    (:string (format nil "~s" value))
    ((:char :unsigned-char) (format nil "~c (~d)" value value))
    (:boolean (if value "T" "NIL"))
    (:void "void")
    (t (format nil "#<unknown-type ~a>" value))))


(defmethod cffi:translate-name-to-foreign ((spec symbol)
                                           (package (eql (find-package :cl-gccjit-ffi)))
                                           &optional varp)
  (declare (ignore varp))
  (format nil "gcc_jit_~a" (cffi:translate-underscore-separated-name spec)))


(defmacro defcfun-debug-logging (name rettype &body args)
  "Wrapper around `cffi:defcfun' that adds debug logging when `*debug-logging*' is T.
`name' will be translated to the C function name by prefixing it with 'gcc_jit_'.
"
  (let* ((lisp-name name)
         (c-name (cffi:translate-name-to-foreign name *package*)))
    (let ((arg-names (mapcar #'first args))
          (arg-types (mapcar #'second args))
          (internal-name (gensym (concatenate 'string (symbol-name lisp-name) "-INTERNAL"))))
      `(progn
         (cffi:defcfun (,c-name ,internal-name)
             ,rettype ,@args)

         (defun ,lisp-name (,@arg-names)
           (let ((result (,internal-name ,@arg-names)))
             (when *debug-logging*
               (format *debug-logging-destination*
                       "FFI CALL: ~a(~{~a~^, ~}) -> ~a~%"
                       ',lisp-name
                       (mapcar #'format-ffi-arg ',arg-types (list ,@arg-names))
                       (format-ffi-arg ',rettype result)))
             result))))))
