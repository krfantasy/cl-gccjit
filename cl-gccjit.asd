(in-package :asdf-user)

(defsystem cl-gccjit
  :description "libgccjit binding"
  :version "0.0.0"
  :license "LGPLv3"
  :author "Jay Xu <jay.xu.krfantasy@gmail.com>"
  :depends-on (:alexandria
               :cffi)
  :serial t
  :components ((:file "packages")
               (:file "load")
               (:file "debug")
               (:file "ffi"))
  :in-order-to ((test-op (test-op :cl-gccjit/tests))))

(defsystem cl-gccjit/tests
  :description "Test suite for cl-gccjit"
  :author "Jay Xu <jay.xu.krfantasy@gmail.com>"
  :license "LGPLv3"
  :depends-on (:cl-gccjit :fiveam)
  :serial t
  :pathname "test"
  :components ((:file "package")
               (:file "square")
               (:file "loop")
               (:file "interp"))
  :perform (test-op (op c)
    (symbol-call :cl-gccjit-tests '#:run-tests!)))
