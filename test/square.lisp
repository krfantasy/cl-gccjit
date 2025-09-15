(in-package :cl-gccjit-tests)

(def-suite cl-gccjit-suite
  :description "Test suite for cl-gccjit")

(in-suite cl-gccjit-suite)

;; reuse the code from example
(load (asdf:system-relative-pathname :cl-gccjit "examples/square/square.lisp"))

(test square-function
  "Test creating and calling a simple square function."
  (let ((ctxt (gccjit-ffi:context-acquire)))
    (unwind-protect
         (progn
           (create-code ctxt)
           (let ((result (gccjit-ffi:context-compile ctxt)))
             (unwind-protect
                  (let ((fnptr (gccjit-ffi:result-get-code result "square")))
                    (is (not (cffi:null-pointer-p fnptr)))
                    (is (= 25 (cffi:foreign-funcall-pointer fnptr () :int 5 :int)))
                    (is (= 100 (cffi:foreign-funcall-pointer fnptr () :int 10 :int))))
               (gccjit-ffi:result-release result))))
      (gccjit-ffi:context-release ctxt))))

(defun run-tests! ()
  (fiveam:run! 'cl-gccjit-suite))
