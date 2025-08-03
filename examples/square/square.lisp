;;; Tutorial part 2: Creating a trivial machine code function
;;; https://gcc.gnu.org/onlinedocs/gcc-15.1.0/jit/intro/tutorial02.html

(defun create-code (ctxt)
  (let* ((int-type (gccjit-ffi:context-get-type ctxt (cffi:foreign-enum-value 'gccjit-ffi:types :type-int)))
         (param-i (gccjit-ffi:context-new-param ctxt (cffi:null-pointer) int-type "i")))
    (cffi:with-foreign-object (params-ptr :pointer 1)
      (setf (cffi:mem-aref params-ptr :pointer 0) param-i)
      (let* ((func (gccjit-ffi:context-new-function
                    ctxt
                    (cffi:null-pointer)
                    :function-kind-exported
                    int-type
                    "square"
                    1
                    params-ptr
                    0))
             (block% (gccjit-ffi:function-new-block func (cffi:null-pointer)))
             (expr (gccjit-ffi:context-new-binary-op
                    ctxt
                    (cffi:null-pointer)
                    :binary-op-mult
                    int-type
                    (gccjit-ffi:param-as-rvalue param-i)
                    (gccjit-ffi:param-as-rvalue param-i))))
        (gccjit-ffi:block-end-with-return block% (cffi:null-pointer) expr)))))


(let ((ctxt (gccjit-ffi:context-acquire)))
  (gccjit-ffi:context-set-bool-option ctxt :bool-option-dump-generated-code 0)
  (create-code ctxt)
  (let ((result (gccjit-ffi:context-compile ctxt)))
    (gccjit-ffi:context-release ctxt)
    (let ((fnptr (gccjit-ffi:result-get-code result "square")))
      (format t "square(5) = ~a~%" (cffi:foreign-funcall-pointer fnptr () :int 5 :int)))
    (gccjit-ffi:result-release result)))
