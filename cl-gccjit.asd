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
               (:file "ffi")))
