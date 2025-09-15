(in-package :cl-gccjit-tests)

(in-suite cl-gccjit-suite)

;; reuse the code from example
(load (asdf:system-relative-pathname :cl-gccjit "examples/interp/interp.lisp"))

(in-package :cl-gccjit-tests)

(test interpreter-fibonacci-function
  "Test the interpreter with the Fibonacci function."
  (let* ((fib-fn (cl-user::create-fibonacci-function))
         (compiled (cl-user::toyvm-function-compile fib-fn)))
    (unwind-protect
         (progn
           (is (= 0 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 0 :int)))
           (is (= 1 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 1 :int)))
           (is (= 34 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 9 :int))))
      (gccjit-ffi:result-release (cl-user::toyvm-compiled-function-jit-result compiled)))))

(test interpreter-arithmetic-function
  "Test the interpreter with a simple arithmetic function."
  (let* ((arith-fn (cl-user::create-simple-arithmetic-function))
         (compiled (cl-user::toyvm-function-compile arith-fn)))
    (unwind-protect
         (progn
           (is (= -15 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 0 :int)))
           (is (= 0 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 3 :int)))
           (is (= 20 (cffi:foreign-funcall-pointer (cl-user::toyvm-compiled-function-code compiled) () :int 5 :int))))
      (gccjit-ffi:result-release (cl-user::toyvm-compiled-function-jit-result compiled)))))