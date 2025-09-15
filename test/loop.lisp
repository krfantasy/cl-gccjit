(in-package :cl-gccjit-tests)

(in-suite cl-gccjit-suite)

;; reuse the code from example
(load (asdf:system-relative-pathname :cl-gccjit "examples/loop/loop.lisp"))

(test loop-test-function
  "Test creating and calling a function with a loop."
  (let ((ctxt (gccjit-ffi:context-acquire)))
    (unwind-protect
         (progn
           (create-loop-code ctxt)
           (let ((result (gccjit-ffi:context-compile ctxt)))
             (unwind-protect
                  (let ((fnptr (gccjit-ffi:result-get-code result "loop_test")))
                    (is (not (cffi:null-pointer-p fnptr)))
                    (is (= 285 (cffi:foreign-funcall-pointer fnptr () :int 10 :int))))
               (gccjit-ffi:result-release result))))
      (gccjit-ffi:context-release ctxt))))
