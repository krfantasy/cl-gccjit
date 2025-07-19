(in-package #:cl-gccjit-ffi)

(cffi:define-foreign-library libgccjit
  (:darwin "libgccjit.dylib")
  (:unix "libgccjit.so")
  (t "libgccjit"))

(cffi:use-foreign-library libgccjit)

;;; Data structures
(defctype gcc-jit-context :pointer)
(defctype gcc-jit-result :pointer)

(defctype gcc-jit-object :pointer)
(defctype gcc-jit-location :pointer)
(defctype gcc-jit-type :pointer)
(defctype gcc-jit-field :pointer)
(defctype gcc-jit-struct :pointer)
(defctype gcc-jit-function-type :pointer)
(defctype gcc-jit-vector-type :pointer)
(defctype gcc-jit-function :pointer)
(defctype gcc-jit-block :pointer)
(defctype gcc-jit-rvalue :pointer)
(defctype gcc-jit-lvalue :pointer)
(defctype gcc-jit-param :pointer)
(defctype gcc-jit-case :pointer)
(defctype gcc-jit-extended-asm :pointer)

;;; Context
(defcfun gcc-jit-context-acquire gcc-jit-context)

(defcfun gcc-jit-context-release :void
  (ctxt gcc-jit-context))

(defcenum gcc-jit-str-option
  :progname
  :special-chars-in-func-names
  :num-str-options)

(defcenum gcc-jit-int-option
  :optimization-level
  :num-int-options)

(defcenum gcc-jit-bool-option
  :debuginfo
  :dump-initial-tree
  :dump-initial-gimple
  :dump-generated-code
  :dump-summary
  :dump-everything
  :selfcheck-gc
  :keep-intermediates
  :num-bool-options)

(defcfun gcc-jit-context-set-str-option :void
  (ctxt gcc-jit-context)
  (opt gcc-jit-str-option)
  (value :string))

(defcfun gcc-jit-context-set-int-option :void
  (ctxt gcc-jit-context)
  (opt gcc-jit-int-option)
  (value :int))

(defcfun gcc-jit-context-set-bool-option :void
  (ctxt gcc-jit-context)
  (opt gcc-jit-bool-option)
  (value :int))

;#+gcc-jit-allow-unreachable-blocks
(defcfun gcc-jit-context-set-bool-allow-unreachable-blocks :void
  (ctxt gcc-jit-context)
  (bool-value :int))

;#+gcc-jit-print-errors-to-stderr
(defcfun gcc-jit-context-set-bool-print-errors-to-stderr :void
  (ctxt gcc-jit-context)
  (enabled :int))

;#+gcc-jit-use-external-driver
(defcfun gcc-jit-context-set-bool-use-external-driver :void
  (ctxt gcc-jit-context)
  (bool-value :int))

;#+gcc-jit-command-line-option
(defcfun gcc-jit-context-add-command-line-option :void
  (ctxt gcc-jit-context)
  (optname :string))

;#+gcc-jit-driver-option
(defcfun gcc-jit-context-add-driver-option :void
  (ctxt gcc-jit-context)
  (optname :string))

(defcfun gcc-jit-context-compile gcc-jit-result
  (ctxt gcc-jit-context))

(defcenum gcc-jit-output-kind
  :assmebler
  :object-file
  :dynamic-library
  :executable)

(defcfun gcc-jit-context-compile-to-file :void
  (ctxt gcc-jit-context)
  (output-kind gcc-jit-output-kind)
  (output-path :string))

(defcfun gcc-jit-context-dump-to-file :void
  (ctxt gcc-jit-context)
  (path :string)
  (update-locations :int))

(defcfun gcc-jit-context-set-logfile :void
  (ctxt gcc-jit-context)
  (logfile :pointer)
  (flags :int)
  (verbosity :int))

(defcfun gcc-jit-context-get-first-error :string
  (ctxt gcc-jit-context))

(defcfun gcc-jit-context-get-last-error :string
  (ctxt gcc-jit-context))

(defcfun gcc-jit-result-get-code :pointer
  (result gcc-jit-result)
  (funcname :string))

(defcfun gcc-jit-result-get-global :pointer
  (result gcc-jit-result)
  (name :string))

(defcfun gcc-jit-result-release :void
  (result gcc-jit-result))

;;; Objects
(defcfun gcc-jit-object-get-context gcc-jit-context
  (obj gcc-jit-object))

(defcfun gcc-jit-object-get-debug-string :string
  (obj gcc-jit-object))

;;; Debugging information
(defcfun gcc-jit-context-new-location gcc-jit-location
  (ctxt gcc-jit-context)
  (filename :string)
  (line :int)
  (column :int))

(defcfun gcc-jit-location-as-object gcc-jit-object
  (loc gcc-jit-location))

;;; Types
(defcfun gcc-jit-type-as-object gcc-jit-object
  (type gcc-jit-type))

(defcenum gcc-jit-types
  :void                                 ; C's "void" type
  :void-ptr                             ; "void *"

  ;; C++'s bool type and C99 "_Bool" type, aka "bool" if using stdbool.h
  :bool

  ;; C's "char" and the variants
  :char
  :signed-char
  :unsigned-int-char

  ;; C's integer types
  :short
  :unsigned-int-short
  :int
  :unsigned-int-int
  :long
  :unsigned-int-long
  :long-long
  :unsigned-int-long-long

  ;; floating-point types
  :float
  :double
  :long-double

  ;; C's "const char*"
  :const-char-ptr

  ;; C's "size_t"
  :size-t

  ;; "FILE *" from stdio.h
  :file-ptr

  ;; Complex types
  :complex-float
  :complex-double
  :complex-long-double

  ;; Sized integer types
  :uint8-t
  :uint16-t
  :uint32-t
  :uint64-t
  :uint128-t
  :int8-t
  :int16-t
  :int32-t
  :int64-t
  :int128-t

  :bfloat16)

(defcfun gcc-jit-context-get-type gcc-jit-type
  (ctxt gcc-jit-context)
  (type gcc-jit-types))

(defcfun gcc-jit-context-get-int-type gcc-jit-type
  (ctxt gcc-jit-context)
  (num-bytes :int)
  (is-signed :int))

(defcfun gcc-jit-type-get-pointer gcc-jit-type
  (type gcc-jit-type))

(defcfun gcc-jit-type-get-const gcc-jit-type
  (type gcc-jit-type))


(defcfun gcc-jit-type-get-volatile gcc-jit-type
  (type gcc-jit-type))

;#+gcc-jit-type-get-restrict
(defcfun gcc-jit-type-get-restrict gcc-jit-type
  (type gcc-jit-type))

;#+gcc-jit-sized-integers
(defcfun gcc-jit-compatible-types :int
  (ltype gcc-jit-type)
  (rtype gcc-jit-type))

;#+gcc-jit-sized-integers
(defcfun gcc-jit-type-get-size :ssize
  (type gcc-jit-type))

(defcfun gcc-jit-context-new-array-type gcc-jit-type
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (element-type gcc-jit-type)
  (num-elements :int))

(defcfun gcc-jit-context-new-field gcc-jit-field
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (name :string))

;#+gcc-jit-context-new-bitfield
(defcfun gcc-jit-context-new-bitfield gcc-jit-field
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (width :int)
  (name :string))

(defcfun gcc-jit-field-as-object gcc-jit-object
  (field gcc-jit-field))

(defcfun gcc-jit-context-new-struct-type gcc-jit-struct
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (name :string)
  (num-fields :int)
  (fields (:pointer gcc-jit-field)))

(defcfun gcc-jit-context-new-opaque-struct gcc-jit-struct
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (name :string))

(defcfun gcc-jit-struct-as-type gcc-jit-type
  (struct-type gcc-jit-struct))

(defcfun gcc-jit-struct-set-fields gcc-jit-struct
  (struct-type gcc-jit-struct)
  (loc gcc-jit-location)
  (num-fields :int)
  (fields (:pointer gcc-jit-field)))

(defcfun gcc-jit-struct-get-field gcc-jit-field
  (struct-type gcc-jit-struct)
  (index :size))

(defcfun gcc-jit-struct-get-field-count :size
  (struct-type gcc-jit-struct))

(defcfun gcc-jit-context-new-union-type gcc-jit-type
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (name :string)
  (num-fields :int)
  (fields (:pointer gcc-jit-field)))

(defcfun gcc-jit-context-new-funciton-ptr-type gcc-jit-type
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (return-type gcc-jit-type)
  (num-params :int)
  (param-types (:pointer gcc-jit-type))
  (is-variadic :int))

;;; Constructing functions
(defcfun gcc-jit-context-new-param gcc-jit-param
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (name :string))

(defcfun gcc-jit-param-as-object gcc-jit-object
  (param gcc-jit-param))

(defcfun gcc-jit-param-as-lvalue gcc-jit-lvalue
  (param gcc-jit-param))

(defcfun gcc-jit-param-as-rvalue gcc-jit-rvalue
  (param gcc-jit-param))

(defcenum gcc-jit-function-kind
  :exported
  :internel
  :imported
  :always-inline)

(defcenum gcc-jit-tls-model
  :none
  :global-dynamic
  :local-dynamic
  :initial-exec
  :local-exec)

(defcfun gcc-jit-context-new-function gcc-jit-function
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (kind gcc-jit-function-kind)
  (return-type gcc-jit-type)
  (name :string)
  (num-params :int)
  (params (:pointer gcc-jit-param))
  (is-variadic :int))

(defcfun gcc-jit-context-get-builtin-function gcc-jit-function
  (ctxt gcc-jit-context)
  (name :string))

(defcfun gcc-jit-function-as-object gcc-jit-object
  (func gcc-jit-function))

(defcfun gcc-jit-function-get-param gcc-jit-param
  (func gcc-jit-function)
  (index :int))

(defcfun gcc-jit-function-dump-to-dot :void
  (func gcc-jit-function)
  (path :string))

;;; Block
(defcfun gcc-jit-function-new-block gcc-jit-block
  (func gcc-jit-function)
  (name :string))

(defcfun gcc-jit-block-as-object gcc-jit-object
  (block_ gcc-jit-block))

(defcfun gcc-jit-block-get-function gcc-jit-function
  (block_ gcc-jit-block))

;;; lvalues, rvalues and expressions
(defcenum gcc-jit-global-kind
  :exported
  :internal
  :imported)

(defcfun gcc-jit-context-new-global gcc-jit-lvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (kind gcc-jit-global-kind)
  (type gcc-jit-type)
  (name :string))

;#+gcc-jit-ctors
(defcfun gcc-jit-context-new-struct-constructor gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (num-values :size)
  (fields (:pointer gcc-jit-field))
  (values (:pointer gcc-jit-rvalue)))

;#+gcc-jit-ctors
(defcfun gcc-jit-context-new-union-constructor gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (field gcc-jit-field)
  (value gcc-jit-rvalue))

;#+gcc-jit-ctors
(defcfun gcc-jit-context-new-array-constructor gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (num-values :size)
  (values (:pointer gcc-jit-rvalue)))

;#+gcc-jit-ctors
(defcfun gcc-jit-global-set-initializer-rvalue gcc-jit-lvalue
  (ctxt gcc-jit-lvalue)
  (init-value gcc-jit-rvalue))

;#+gcc-jit-get-target-builtin-function
(defcfun gcc-jit-context-get-target-builtin-funciton gcc-jit-function
  (ctxt gcc-jit-context)
  (name :string))

;#+gcc-jit-global-set-initializer
(defcfun gcc-jit-global-set-initializer gcc-jit-lvalue
  (global gcc-jit-lvalue)
  (blob :pointer)
  (num-bytes :size))

;#+gcc-jit-global-set-readonly
(defcfun gcc-jit-global-set-randomly :void
  (global gcc-jit-lvalue))

(defcfun gcc-jit-lvalue-as-object gcc-jit-object
  (lvalue gcc-jit-lvalue))

(defcfun gcc-jit-lvalue-as-rvalue gcc-jit-rvalue
  (rvalue gcc-jit-lvalue))

(defcfun gcc-jit-rvalue-as-object gcc-jit-object
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-rvalue-get-type gcc-jit-type
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-context-new-rvalue-from-int gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (numeric-type gcc-jit-type)
  (value :int))

(defcfun gcc-jit-context-new-rvalue-from-long gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (numeric-type gcc-jit-type)
  (value :long))

(defcfun gcc-jit-context-zero gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (numeric-type gcc-jit-type))

(defcfun gcc-jit-context-one gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (numeric-type gcc-jit-type))

(defcfun gcc-jit-context-new-rvalue-from-double gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (numeric-type gcc-jit-type)
  (value :double))

(defcfun gcc-jit-context-new-rvalue-from-ptr gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (pointer-type gcc-jit-type)
  (value :pointer))

(defcfun gcc-jit-context-null gcc-jit-rvalue
  (ctxt gcc-jit-context))

;#+gcc-jit-context-new-sizeof
(defcfun gcc-jit-context-new-sizeof gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (type gcc-jit-type))

;#+gcc-jit-context-new-alignof
(defcfun gcc-jit-context-new-alignof gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (type gcc-jit-type))

(defcfun gcc-jit-context-new-string-literal gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (value :string))

(defcenum gcc-jit-unary-op
  :minus
  :bitwise-negate
  :logical-negate
  :abs)

(defcfun gcc-jit-context-new-unary-op gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (op gcc-jit-unary-op)
  (result-type gcc-jit-type)
  (rvalue gcc-jit-rvalue))

(defcenum gcc-jit-binary-op
  :plus
  :minus
  :mult
  :divide
  :modulo
  :bitwise-and
  :bitwise-xor
  :bitwise-or
  :logical-and
  :lgoical-or
  :lshift
  :rshift)

(defcfun gcc-jit-context-new-binary-op gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (op gcc-jit-binary-op)
  (result-type gcc-jit-type)
  (a gcc-jit-rvalue)
  (b gcc-jit-rvalue))

(defcenum gcc-jit-comparison
  :eq
  :ne
  :lt
  :le
  :gt
  :ge)

(defcfun gcc-jit-context-new-comparison gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (op gcc-jit-comparison)
  (a gcc-jit-rvalue)
  (b gcc-jit-rvalue))

(defcfun gcc-jit-context-new-call gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (fn-ptr gcc-jit-rvalue)
  (numargs :int)
  (args (:pointer gcc-jit-rvalue)))

(defcfun gcc-jit-context-new-call-through-ptr gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (fn-ptr gcc-jit-rvalue)
  (numargs :int)
  (args (:pointer gcc-jit-rvalue)))

(defcfun gcc-jit-context-new-cast gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (value gcc-jit-rvalue)
  (type gcc-jit-type))

;#+gcc-jit-context-new-bitcast
(defcfun gcc-jit-context-new-bitcast gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (value gcc-jit-rvalue)
  (type gcc-jit-type))

;;#+gcc-jit-alignment
(defcfun gcc-jit-lvalue-set-alignment :void
  (lvalue gcc-jit-lvalue)
  (bytes :unsigned-int))

(defcfun gcc-jit-lvalue-get-alignment :unsigned-int
  (lvalue gcc-jit-lvalue))

(defcfun gcc-jit-context-new-array-access gcc-jit-lvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (ptr gcc-jit-rvalue)
  (index gcc-jit-rvalue))

;#+gcc-jit-context-convert-vector
(defcfun gcc-jit-context-convert-vector gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (vector gcc-jit-rvalue)
  (type gcc-jit-type))

;#+gcc-jit-vector-operations
(defcfun gcc-jit-context-new-rvalue-vector-perm gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (elements1 gcc-jit-rvalue)
  (elements2 gcc-jit-rvalue)
  (mask gcc-jit-rvalue))

;#+gcc-jit-vector-operations
(defcfun gcc-jit-context-new-vector-access gcc-jit-lvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (vector gcc-jit-rvalue)
  (index gcc-jit-rvalue))

(defcfun gcc-jit-lvalue-access-field gcc-jit-lvalue
  (struct-or-union gcc-jit-lvalue)
  (loc gcc-jit-location)
  (field gcc-jit-field))

(defcfun gcc-jit-rvalue-access-field gcc-jit-rvalue
  (struct-or-union gcc-jit-rvalue)
  (loc gcc-jit-location)
  (field gcc-jit-field))

(defcfun gcc-jit-rvalue-dereference-field gcc-jit-lvalue
  (ptr gcc-jit-rvalue)
  (loc gcc-jit-location)
  (field gcc-jit-field))

(defcfun gcc-jit-rvalue-dereference gcc-jit-lvalue
  (rvalue gcc-jit-rvalue)
  (loc gcc-jit-location))

(defcfun gcc-jit-lvalue-get-address gcc-jit-rvalue
  (lvalue gcc-jit-lvalue)
  (loc gcc-jit-location))

;#+gcc-jit-lvalue-set-tls-model
(defcfun gcc-jit-lvalue-set-tls-model :void
  (lvalue gcc-jit-lvalue)
  (model gcc-jit-tls-model))

;#+gcc-jit-lvalue-set-link-section
(defcfun gcc-jit-lvalue-set-link-section :void
  (lvalue gcc-jit-lvalue)
  (section-name :string))

;#+gcc-jit-lvalue-set-register-name
(defcfun gcc-jit-lvalue-set-register-name :void
  (lvalue gcc-jit-lvalue)
  (reg-name :string))

(defcfun gcc-jit-function-new-local gcc-jit-lvalue
  (func gcc-jit-function)
  (loc gcc-jit-location)
  (type gcc-jit-type)
  (name :string))

(defcfun gcc-jit-function-new-temp gcc-jit-lvalue
  (func gcc-jit-function)
  (loc gcc-jit-location)
  (type gcc-jit-type))

;;; Statement creation
(defcfun gcc-jit-block-add-eval :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-block-add-assignment :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (lvalue gcc-jit-lvalue)
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-block-add-assignment-op :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (lvalue gcc-jit-lvalue)
  (op gcc-jit-binary-op)
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-block-add-comment :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (text :string))

(defcfun gcc-jit-block-end-with-conditional :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (boolval gcc-jit-rvalue)
  (on-true gcc-jit-block)
  (on-false gcc-jit-block))

(defcfun gcc-jit-block-end-with-jump :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (target gcc-jit-block))

(defcfun gcc-jit-block-end-with-return :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (rvalue gcc-jit-rvalue))

(defcfun gcc-jit-block-end-with-void-return :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location))

;#+gcc-jit-switch-statements
(defcfun gcc-jit-context-new-case gcc-jit-case
  (ctxt gcc-jit-context)
  (min-value gcc-jit-rvalue)
  (max-value gcc-jit-rvalue)
  (dest-block gcc-jit-block))

(defcfun gcc-jit-case-as-object gcc-jit-object
  (case_ gcc-jit-case))

(defcfun gcc-jit-block-end-with-switch :void
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (expr gcc-jit-rvalue)
  (default-block gcc-jit-block)
  (num-cases :int)
  (cases (:pointer gcc-jit-case)))

;;; Nested contexts
(defcfun gcc-jit-context-new-child-context gcc-jit-context
  (parent-ctxt gcc-jit-context))

(defcfun gcc-jit-context-dump-reproducer-to-file :void
  (ctxt gcc-jit-context)
  (path :string))

(defcfun gcc-jit-context-enable-dump :void
  (ctxt gcc-jit-context)
  (dumpname :string)
  (out-ptr (:pointer :string)))

;;; Timing support
;#+gcc-jit-timing-api
(defctype gcc-jit-timer :pointer)

(defcfun gcc-jit-timer-new gcc-jit-timer)

(defcfun gcc-jit-timer-release gcc-jit-timer
  (timer gcc-jit-timer))

(defcfun gcc-jit-context-set-timer :void
  (ctxt gcc-jit-context)
  (timer gcc-jit-timer))

(defcfun gcc-jit-context-get-timer gcc-jit-timer
  (ctxt gcc-jit-context))

(defcfun gcc-jit-timer-push :void
  (timer gcc-jit-timer)
  (item-name :string))

(defcfun gcc-jit-timer-pop :void
  (timer gcc-jit-timer)
  (item-name :string))

(defcfun gcc-jit-timer-print :void
  (timer gcc-jit-timer)
  (f-out :pointer))                     ; FILE *

;#+gcc-jit-rvalue-set-bool-require-tail-call
(defcfun gcc-jit-rvalue-set-bool-require-tail-call :void
  (call gcc-jit-rvalue)
  (require-tail-call :int))

;#+gcc-jit-type-get-aligned
(defcfun gcc-jit-type-get-aligned gcc-jit-type
  (type gcc-jit-type)
  (alignment-in-bytes :size))

;#+gcc-jit-type-get-vector
(defcfun gcc-jit-type-get-vector gcc-jit-type
  (type gcc-jit-type)
  (num-units :size))

;#+gcc-jit-function-get-address
(defcfun gcc-jit-function-get-address gcc-jit-rvalue
  (fn gcc-jit-function)
  (loc gcc-jit-location))

;#+gcc-jit-context-new-rvalue-from-vector
(defcfun gcc-jit-context-new-rvalue-from-vector gcc-jit-rvalue
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (vec-type gcc-jit-type)
  (num-elements :size)
  (elements gcc-jit-rvalue))

;#+gcc-jit-version
(defcfun gcc-jit-version-major :int)

(defcfun gcc-jit-version-minor :int)

(defcfun gcc-jit-version-patchlevel :int)

;;; Asm support
;#+gcc-jit-asm-statements
(defcfun gcc-jit-block-add-extended-asm gcc-jit-extended-asm
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (asm-template :string))

(defcfun gcc-jit-block-end-with-extended-asm-goto  gcc-jit-extended-asm
  (block_ gcc-jit-block)
  (loc gcc-jit-location)
  (asm-template :string)
  (num-goto-blocks :int)
  (goto-blocks (:pointer gcc-jit-block))
  (fallthrough-block gcc-jit-block))

(defcfun gcc-jit-extended-asm-as-object gcc-jit-object
  (ext-asm gcc-jit-extended-asm))

(defcfun gcc-jit-extended-asm-set-volatile-flag :void
  (ext-asm gcc-jit-extended-asm)
  (flag :int))

(defcfun gcc-jit-extended-asm-set-inline-flag :void
  (ext-asm gcc-jit-extended-asm)
  (flag :int))

(defcfun gcc-jit-extended-asm-add-output-operand :void
  (ext-asm gcc-jit-extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (dest gcc-jit-lvalue))

(defcfun gcc-jit-extended-asm-add-input-operand :void
  (ext-asm gcc-jit-extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (src gcc-jit-rvalue))

(defcfun gcc-jit-extended-asm-add-clobber :void
  (ext-asm gcc-jit-extended-asm)
  (victim :string))

(defcfun gcc-jit-add-top-level-asm :void
  (ctxt gcc-jit-context)
  (loc gcc-jit-location)
  (asm-stmts :string))

;#+gcc-jit-reflection
(defcfun gcc-jit-function-get-return-type gcc-jit-type
  (func gcc-jit-function))

(defcfun gcc-jit-function-get-param-count :size
  (func gcc-jit-function))

(defcfun gcc-jit-type-dyncast-array gcc-jit-type
  (type gcc-jit-type))

(defcfun gcc-jit-type-is-bool :int
  (type gcc-jit-type))

(defcfun gcc-jit-type-dyncast-function-ptr-type gcc-jit-function-type
  (type gcc-jit-type))

(defcfun gcc-jit-function-type-get-return-type gcc-jit-type
  (function-type gcc-jit-function-type))

(defcfun gcc-jit-function-type-get-param-count :size
  (function-type gcc-jit-function-type))

(defcfun gcc-jit-function-type-get-param-type gcc-jit-type
  (function-type gcc-jit-function-type)
  (index :size))

(defcfun gcc-jit-type-is-integral :int
  (type gcc-jit-type))

(defcfun gcc-jit-type-is-pointer gcc-jit-type
  (type gcc-jit-type))

(defcfun gcc-jit-type-dyncast-vector gcc-jit-vector-type
  (type gcc-jit-type))

(defcfun gcc-jit-type-is-struct gcc-jit-struct
  (type gcc-jit-type))

(defcfun gcc-jit-vector-type-get-num-units :size
  (vector-type gcc-jit-vector-type))

(defcfun gcc-jit-vector-type-get-element-type gcc-jit-type
  (vector-type gcc-jit-vector-type))

(defcfun gcc-jit-type-unqualified gcc-jit-type
  (type gcc-jit-type))

(defcenum gcc-jit-fn-attribute
  :alias
  :always-inline
  :inline
  :noinline
  :target
  :used
  :visibility
  :cold
  :returns-twice
  :pure
  :const
  :weak
  :nonnull
  :max                                  ; just for indicate the maximum value of this enum
  )

(defcfun gcc-jit-funciton-add-attribute :void
  (func gcc-jit-function)
  (attribute gcc-jit-fn-attribute))

(defcfun gcc-jit-function-add-string-attribute :void
  (func gcc-jit-function)
  (attribute gcc-jit-fn-attribute)
  (value :string))

(defcfun gcc-jit-function-add-integer-array-attribute :void
  (func gcc-jit-function)
  (attribute gcc-jit-fn-attribute)
  (value (:pointer :int))
  (length :size))

(defcenum gcc-jit-variable-attribute
  :visibility
  :max)

(defcfun gcc-jit-lvalue-add-string-attribute :void
  (variable gcc-jit-lvalue)
  (attribute gcc-jit-variable-attribute)
  (value :string))

;#+gcc-jit-context-set-output-ident
(defcfun gcc-jit-context-set-output-ident :void
  (ctxt gcc-jit-context)
  (output-ident :string))
