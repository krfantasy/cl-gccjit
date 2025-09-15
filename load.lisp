(in-package :cl-user)

(cffi:define-foreign-library libgccjit
  (:darwin (:or "libgccjit.dylib" "libgccjit.0.dylib"))
  (:unix (:or "libgccjit.so" "libgccjit.so.0"))
  (t "libgccjit"))

#+darwin
(pushnew "/opt/homebrew/lib/gcc/current/lib" cffi:*foreign-library-directories*)

#+linux
(progn
  (pushnew "/usr/lib/x86_64-linux-gnu" cffi:*foreign-library-directories*)
  (pushnew "/usr/lib/aarch64-linux-gnu" cffi:*foreign-library-directories*)
  (pushnew "/usr/lib" cffi:*foreign-library-directories*))

(cffi:use-foreign-library libgccjit)

(defmethod cffi:translate-name-to-foreign ((spec symbol)
                                           (package (eql (find-package :cl-gccjit-ffi)))
                                           &optional varp)
  (declare (ignore varp))
  (format nil "gcc_jit_~a" (cffi:translate-underscore-separated-name spec)))
