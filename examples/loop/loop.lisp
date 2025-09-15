;;; Tutorial part 3: Creating a function with a loop
;;; https://gcc.gnu.org/onlinedocs/jit/intro/tutorial03.html

(defun create-loop-code (ctxt)
  "Create a function that computes the sum of squares from 0 to n-1.
   
   int loop_test (int n)
   {
     int i;
     int sum = 0;
     for (i = 0; i < n ; i ++)
     {
       sum += i * i;
     }
     return sum;
   }"
  (let* ((the-type (gccjit-ffi:context-get-type ctxt (cffi:foreign-enum-value 'gccjit-ffi:types :type-int)))
         (return-type the-type)
         (n (gccjit-ffi:context-new-param ctxt (cffi:null-pointer) the-type "n")))
    (cffi:with-foreign-object (params-ptr :pointer 1)
      (setf (cffi:mem-aref params-ptr :pointer 0) n)
      (let* ((func (gccjit-ffi:context-new-function
                    ctxt
                    (cffi:null-pointer)
                    :function-kind-exported
                    return-type
                    "loop_test"
                    1
                    params-ptr
                    0))
             ;; Build locals
             (i (gccjit-ffi:function-new-local func (cffi:null-pointer) the-type "i"))
             (sum (gccjit-ffi:function-new-local func (cffi:null-pointer) the-type "sum"))
             (b-initial (gccjit-ffi:function-new-block func "initial"))
             (b-loop-cond (gccjit-ffi:function-new-block func "loop_cond"))
             (b-loop-body (gccjit-ffi:function-new-block func "loop_body"))
             (b-after-loop (gccjit-ffi:function-new-block func "after_loop")))
        
        ;; sum = 0;
        (gccjit-ffi:block-add-assignment
         b-initial (cffi:null-pointer)
         sum
         (gccjit-ffi:context-zero ctxt the-type))
        
        ;; i = 0;
        (gccjit-ffi:block-add-assignment
         b-initial (cffi:null-pointer)
         i
         (gccjit-ffi:context-zero ctxt the-type))
        
        (gccjit-ffi:block-end-with-jump b-initial (cffi:null-pointer) b-loop-cond)
        
        ;; if (i >= n)
        (gccjit-ffi:block-end-with-conditional
         b-loop-cond (cffi:null-pointer)
         (gccjit-ffi:context-new-comparison
          ctxt (cffi:null-pointer)
          :comparison-ge
          (gccjit-ffi:lvalue-as-rvalue i)
          (gccjit-ffi:param-as-rvalue n))
         b-after-loop
         b-loop-body)
        
        ;; sum += i * i
        (gccjit-ffi:block-add-assignment-op
         b-loop-body (cffi:null-pointer)
         sum
         :binary-op-plus
         (gccjit-ffi:context-new-binary-op
          ctxt (cffi:null-pointer)
          :binary-op-mult the-type
          (gccjit-ffi:lvalue-as-rvalue i)
          (gccjit-ffi:lvalue-as-rvalue i)))
        
        ;; i++
        (gccjit-ffi:block-add-assignment-op
         b-loop-body (cffi:null-pointer)
         i
         :binary-op-plus
         (gccjit-ffi:context-one ctxt the-type))
        
        (gccjit-ffi:block-end-with-jump b-loop-body (cffi:null-pointer) b-loop-cond)
        
        ;; return sum
        (gccjit-ffi:block-end-with-return
         b-after-loop
         (cffi:null-pointer)
         (gccjit-ffi:lvalue-as-rvalue sum))))))

#|
;; Main execution
(let ((ctxt (gccjit-ffi:context-acquire)))
  (unwind-protect
       (progn
         ;; Set some options on the context.
         ;; Let's see the code being generated, in assembler form.
         (gccjit-ffi:context-set-bool-option ctxt :bool-option-dump-generated-code 0)
         
         ;; Populate the context.
         (create-loop-code ctxt)
         
         ;; Compile the code.
         (let ((result (gccjit-ffi:context-compile ctxt)))
           (unwind-protect
                (progn
                  ;; Extract the generated code from "result".
                  (let ((loop-test (gccjit-ffi:result-get-code result "loop_test")))
                    (when (not (cffi:null-pointer-p loop-test))
                      ;; Run the generated code.
                      (let ((val (cffi:foreign-funcall-pointer loop-test () :int 10 :int)))
                        (format t "loop_test(10) returned: ~a~%" val)))))
             (gccjit-ffi:result-release result))))
    (gccjit-ffi:context-release ctxt)))
|#
