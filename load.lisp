(in-package :cl-user)

(cffi:define-foreign-library libgccjit
  (:darwin "libgccjit.dylib")
  (:unix "libgccjit.so")
  (t "libgccjit"))

#+darwin
(pushnew "/opt/homebrew/lib/gcc/current/lib" cffi:*foreign-library-directories*)

(cffi:use-foreign-library libgccjit)

(defmethod cffi:translate-name-to-foreign ((spec symbol)
                                           (package (eql (find-package :cl-gccjit-ffi)))
                                           &optional varp)
  (declare (ignore varp))
  (format nil "gcc_jit_~a" (cffi:translate-underscore-separated-name spec)))
