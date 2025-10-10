(in-package #:cl-gccjit-ffi)

;;; Data structures
(defctype context :pointer)
(defctype result :pointer)

(defctype object :pointer)
(defctype location :pointer)
(defctype type :pointer)
(defctype field :pointer)
(defctype struct :pointer)
(defctype function-type :pointer)
(defctype vector-type :pointer)
(defctype function :pointer)
(defctype block :pointer)
(defctype rvalue :pointer)
(defctype lvalue :pointer)
(defctype param :pointer)
(defctype case :pointer)
(defctype extended-asm :pointer)

;;; Context
(defcfun-debug-logging context-acquire context)

(defcfun-debug-logging context-release :void
  (ctxt context))

(defcenum str-option
  :str-option-progname
  :str-option-special-chars-in-func-names
  :num-str-options)

(defcenum int-option
  :int-option-optimization-level
  :num-int-options)

(defcenum bool-option
  :bool-option-debuginfo
  :bool-option-dump-initial-tree
  :bool-option-dump-initial-gimple
  :bool-option-dump-generated-code
  :bool-option-dump-summary
  :bool-option-dump-everything
  :bool-option-selfcheck-gc
  :bool-option-keep-intermediates
  :num-bool-options)

(defcfun-debug-logging context-set-str-option :void
  (ctxt context)
  (opt str-option)
  (value :string))

(defcfun-debug-logging context-set-int-option :void
  (ctxt context)
  (opt int-option)
  (value :int))

(defcfun-debug-logging context-set-bool-option :void
  (ctxt context)
  (opt bool-option)
  (value :int))

;#+allow-unreachable-blocks
(defcfun-debug-logging context-set-bool-allow-unreachable-blocks :void
  (ctxt context)
  (bool-value :int))

;#+print-errors-to-stderr
(defcfun-debug-logging context-set-bool-print-errors-to-stderr :void
  (ctxt context)
  (enabled :int))

;#+use-external-driver
(defcfun-debug-logging context-set-bool-use-external-driver :void
  (ctxt context)
  (bool-value :int))

;#+command-line-option
(defcfun-debug-logging context-add-command-line-option :void
  (ctxt context)
  (optname :string))

;#+driver-option
(defcfun-debug-logging context-add-driver-option :void
  (ctxt context)
  (optname :string))

(defcfun-debug-logging context-compile result
  (ctxt context))

(defcenum output-kind
  :output-kind-assembler
  :output-kind-object-file
  :output-kind-dynamic-library
  :output-kind-executable)

(defcfun-debug-logging context-compile-to-file :void
  (ctxt context)
  (output-kind output-kind)
  (output-path :string))

(defcfun-debug-logging context-dump-to-file :void
  (ctxt context)
  (path :string)
  (update-locations :int))

(defcfun-debug-logging context-set-logfile :void
  (ctxt context)
  (logfile :pointer)
  (flags :int)
  (verbosity :int))

(defcfun-debug-logging context-get-first-error :string
  (ctxt context))

(defcfun-debug-logging context-get-last-error :string
  (ctxt context))

(defcfun-debug-logging result-get-code :pointer
  (result result)
  (funcname :string))

(defcfun-debug-logging result-get-global :pointer
  (result result)
  (name :string))

(defcfun-debug-logging result-release :void
  (result result))

;;; Objects
(defcfun-debug-logging object-get-context context
  (obj object))

(defcfun-debug-logging object-get-debug-string :string
  (obj object))

;;; Debugging information
(defcfun-debug-logging context-new-location location
  (ctxt context)
  (filename :string)
  (line :int)
  (column :int))

(defcfun-debug-logging location-as-object object
  (loc location))

;;; Types
(defcfun-debug-logging type-as-object object
  (type type))

(defcenum types
  :type-void                                 ; C's "void" type
  :type-void-ptr                             ; "void *"

  ;; C++'s bool type and C99 "_Bool" type, aka "bool" if using stdbool.h
  :type-bool

  ;; C's "char" and the variants
  :type-char
  :type-signed-char
  :type-unsigned-char

  ;; C's integer types
  :type-short
  :type-unsigned-short
  :type-int
  :type-unsigned-int
  :type-long
  :type-unsigned-long
  :type-long-long
  :type-unsigned-long-long

  ;; floating-point types
  :type-float
  :type-double
  :type-long-double

  ;; C's "const char*"
  :type-const-char-ptr

  ;; C's "size_t"
  :type-size-t

  ;; "FILE *" from stdio.h
  :type-file-ptr

  ;; Complex types
  :type-complex-float
  :type-complex-double
  :type-complex-long-double

  ;; Sized integer types
  :type-uint8-t
  :type-uint16-t
  :type-uint32-t
  :type-uint64-t
  :type-uint128-t
  :type-int8-t
  :type-int16-t
  :type-int32-t
  :type-int64-t
  :type-int128-t

  :type-bfloat16)

(defcfun-debug-logging context-get-type type
  (ctxt context)
  (type types))

(defcfun-debug-logging context-get-int-type type
  (ctxt context)
  (num-bytes :int)
  (is-signed :int))

(defcfun-debug-logging type-get-pointer type
  (type type))

(defcfun-debug-logging type-get-const type
  (type type))


(defcfun-debug-logging type-get-volatile type
  (type type))

;#+type-get-restrict
(defcfun-debug-logging type-get-restrict type
  (type type))

;#+sized-integers
(defcfun-debug-logging compatible-types :int
  (ltype type)
  (rtype type))

;#+sized-integers
(defcfun-debug-logging type-get-size :ssize
  (type type))

(defcfun-debug-logging context-new-array-type type
  (ctxt context)
  (loc location)
  (element-type type)
  (num-elements :int))

(defcfun-debug-logging context-new-field field
  (ctxt context)
  (loc location)
  (type type)
  (name :string))

;#+context-new-bitfield
(defcfun-debug-logging context-new-bitfield field
  (ctxt context)
  (loc location)
  (type type)
  (width :int)
  (name :string))

(defcfun-debug-logging field-as-object object
  (field field))

(defcfun-debug-logging context-new-struct-type struct
  (ctxt context)
  (loc location)
  (name :string)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun-debug-logging context-new-opaque-struct struct
  (ctxt context)
  (loc location)
  (name :string))

(defcfun-debug-logging struct-as-type type
  (struct-type struct))

(defcfun-debug-logging struct-set-fields struct
  (struct-type struct)
  (loc location)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun-debug-logging struct-get-field field
  (struct-type struct)
  (index :size))

(defcfun-debug-logging struct-get-field-count :size
  (struct-type struct))

(defcfun-debug-logging context-new-union-type type
  (ctxt context)
  (loc location)
  (name :string)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun-debug-logging context-new-function-ptr-type type
  (ctxt context)
  (loc location)
  (return-type type)
  (num-params :int)
  (param-types (:pointer type))
  (is-variadic :int))

;;; Constructing functions
(defcfun-debug-logging context-new-param param
  (ctxt context)
  (loc location)
  (type type)
  (name :string))

(defcfun-debug-logging param-as-object object
  (param param))

(defcfun-debug-logging param-as-lvalue lvalue
  (param param))

(defcfun-debug-logging param-as-rvalue rvalue
  (param param))

(defcenum function-kind
  :function-kind-exported
  :function-kind-internal
  :function-kind-imported
  :function-kind-always-inline)

(defcenum tls-model
  :tls-model-none
  :tls-model-global-dynamic
  :tls-model-local-dynamic
  :tls-model-initial-exec
  :tls-model-local-exec)

(defcfun-debug-logging context-new-function function
  (ctxt context)
  (loc location)
  (kind function-kind)
  (return-type type)
  (name :string)
  (num-params :int)
  (params (:pointer param))
  (is-variadic :int))

(defcfun-debug-logging context-get-builtin-function function
  (ctxt context)
  (name :string))

(defcfun-debug-logging function-as-object object
  (func function))

(defcfun-debug-logging function-get-param param
  (func function)
  (index :int))

(defcfun-debug-logging function-dump-to-dot :void
  (func function)
  (path :string))

;;; Block
(defcfun-debug-logging function-new-block block
  (func function)
  (name :string))

(defcfun-debug-logging block-as-object object
  (block_ block))

(defcfun-debug-logging block-get-function function
  (block_ block))

;;; lvalues, rvalues and expressions
(defcenum global-kind
  :global-kind-exported
  :global-kind-internal
  :global-kind-imported)

(defcfun-debug-logging context-new-global lvalue
  (ctxt context)
  (loc location)
  (kind global-kind)
  (type type)
  (name :string))

;#+ctors
(defcfun-debug-logging context-new-struct-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (num-values :size)
  (fields (:pointer field))
  (values (:pointer rvalue)))

;#+ctors
(defcfun-debug-logging context-new-union-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (field field)
  (value rvalue))

;#+ctors
(defcfun-debug-logging context-new-array-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (num-values :size)
  (values (:pointer rvalue)))

;#+ctors
(defcfun-debug-logging global-set-initializer-rvalue lvalue
  (ctxt lvalue)
  (init-value rvalue))

;#+get-target-builtin-function
(defcfun-debug-logging context-get-target-builtin-function function
  (ctxt context)
  (name :string))

;#+global-set-initializer
(defcfun-debug-logging global-set-initializer lvalue
  (global lvalue)
  (blob :pointer)
  (num-bytes :size))

;#+global-set-readonly
(defcfun-debug-logging global-set-readonly :void
  (global lvalue))

(defcfun-debug-logging lvalue-as-object object
  (lvalue lvalue))

(defcfun-debug-logging lvalue-as-rvalue rvalue
  (lvalue lvalue))

(defcfun-debug-logging rvalue-as-object object
  (rvalue rvalue))

(defcfun-debug-logging rvalue-get-type type
  (rvalue rvalue))

(defcfun-debug-logging context-new-rvalue-from-int rvalue
  (ctxt context)
  (numeric-type type)
  (value :int))

(defcfun-debug-logging context-new-rvalue-from-long rvalue
  (ctxt context)
  (numeric-type type)
  (value :long))

(defcfun-debug-logging context-zero rvalue
  (ctxt context)
  (numeric-type type))

(defcfun-debug-logging context-one rvalue
  (ctxt context)
  (numeric-type type))

(defcfun-debug-logging context-new-rvalue-from-double rvalue
  (ctxt context)
  (numeric-type type)
  (value :double))

(defcfun-debug-logging context-new-rvalue-from-ptr rvalue
  (ctxt context)
  (pointer-type type)
  (value :pointer))

(defcfun-debug-logging context-null rvalue
  (ctxt context))

;#+context-new-sizeof
(defcfun-debug-logging context-new-sizeof rvalue
  (ctxt context)
  (type type))

;#+context-new-alignof
(defcfun-debug-logging context-new-alignof rvalue
  (ctxt context)
  (type type))

(defcfun-debug-logging context-new-string-literal rvalue
  (ctxt context)
  (value :string))

(defcenum unary-op
  :unary-op-minus
  :unary-op-bitwise-negate
  :unary-op-logical-negate
  :unary-op-abs)

(defcfun-debug-logging context-new-unary-op rvalue
  (ctxt context)
  (loc location)
  (op unary-op)
  (result-type type)
  (rvalue rvalue))

(defcenum binary-op
  :binary-op-plus
  :binary-op-minus
  :binary-op-mult
  :binary-op-divide
  :binary-op-modulo
  :binary-op-bitwise-and
  :binary-op-bitwise-xor
  :binary-op-bitwise-or
  :binary-op-logical-and
  :binary-op-logical-or
  :binary-op-lshift
  :binary-op-rshift)

(defcfun-debug-logging context-new-binary-op rvalue
  (ctxt context)
  (loc location)
  (op binary-op)
  (result-type type)
  (a rvalue)
  (b rvalue))

(defcenum comparison
  :comparison-eq
  :comparison-ne
  :comparison-lt
  :comparison-le
  :comparison-gt
  :comparison-ge)

(defcfun-debug-logging context-new-comparison rvalue
  (ctxt context)
  (loc location)
  (op comparison)
  (a rvalue)
  (b rvalue))

(defcfun-debug-logging context-new-call rvalue
  (ctxt context)
  (loc location)
  (fn-ptr rvalue)
  (numargs :int)
  (args (:pointer rvalue)))

(defcfun-debug-logging context-new-call-through-ptr rvalue
  (ctxt context)
  (loc location)
  (fn-ptr rvalue)
  (numargs :int)
  (args (:pointer rvalue)))

(defcfun-debug-logging context-new-cast rvalue
  (ctxt context)
  (loc location)
  (value rvalue)
  (type type))

;#+context-new-bitcast
(defcfun-debug-logging context-new-bitcast rvalue
  (ctxt context)
  (loc location)
  (value rvalue)
  (type type))

;;#+alignment
(defcfun-debug-logging lvalue-set-alignment :void
  (lvalue lvalue)
  (bytes :unsigned-int))

(defcfun-debug-logging lvalue-get-alignment :unsigned-int
  (lvalue lvalue))

(defcfun-debug-logging context-new-array-access lvalue
  (ctxt context)
  (loc location)
  (ptr rvalue)
  (index rvalue))

;#+context-convert-vector
(defcfun-debug-logging context-convert-vector rvalue
  (ctxt context)
  (loc location)
  (vector rvalue)
  (type type))

;#+vector-operations
(defcfun-debug-logging context-new-rvalue-vector-perm rvalue
  (ctxt context)
  (loc location)
  (elements1 rvalue)
  (elements2 rvalue)
  (mask rvalue))

;#+vector-operations
(defcfun-debug-logging context-new-vector-access lvalue
  (ctxt context)
  (loc location)
  (vector rvalue)
  (index rvalue))

(defcfun-debug-logging lvalue-access-field lvalue
  (struct-or-union lvalue)
  (loc location)
  (field field))

(defcfun-debug-logging rvalue-access-field rvalue
  (struct-or-union rvalue)
  (loc location)
  (field field))

(defcfun-debug-logging rvalue-dereference-field lvalue
  (ptr rvalue)
  (loc location)
  (field field))

(defcfun-debug-logging rvalue-dereference lvalue
  (rvalue rvalue)
  (loc location))

(defcfun-debug-logging lvalue-get-address rvalue
  (lvalue lvalue)
  (loc location))

;#+lvalue-set-tls-model
(defcfun-debug-logging lvalue-set-tls-model :void
  (lvalue lvalue)
  (model tls-model))

;#+lvalue-set-link-section
(defcfun-debug-logging lvalue-set-link-section :void
  (lvalue lvalue)
  (section-name :string))

;#+lvalue-set-register-name
(defcfun-debug-logging lvalue-set-register-name :void
  (lvalue lvalue)
  (reg-name :string))

(defcfun-debug-logging function-new-local lvalue
  (func function)
  (loc location)
  (type type)
  (name :string))

(defcfun-debug-logging function-new-temp lvalue
  (func function)
  (loc location)
  (type type))

;;; Statement creation
(defcfun-debug-logging block-add-eval :void
  (block_ block)
  (loc location)
  (rvalue rvalue))

(defcfun-debug-logging block-add-assignment :void
  (block_ block)
  (loc location)
  (lvalue lvalue)
  (rvalue rvalue))

(defcfun-debug-logging block-add-assignment-op :void
  (block_ block)
  (loc location)
  (lvalue lvalue)
  (op binary-op)
  (rvalue rvalue))

(defcfun-debug-logging block-add-comment :void
  (block_ block)
  (loc location)
  (text :string))

(defcfun-debug-logging block-end-with-conditional :void
  (block_ block)
  (loc location)
  (boolval rvalue)
  (on-true block)
  (on-false block))

(defcfun-debug-logging block-end-with-jump :void
  (block_ block)
  (loc location)
  (target block))

(defcfun-debug-logging block-end-with-return :void
  (block_ block)
  (loc location)
  (rvalue rvalue))

(defcfun-debug-logging block-end-with-void-return :void
  (block_ block)
  (loc location))

;#+switch-statements
(defcfun-debug-logging context-new-case case
  (ctxt context)
  (min-value rvalue)
  (max-value rvalue)
  (dest-block block))

(defcfun-debug-logging case-as-object object
  (case_ case))

(defcfun-debug-logging block-end-with-switch :void
  (block_ block)
  (loc location)
  (expr rvalue)
  (default-block block)
  (num-cases :int)
  (cases (:pointer case)))

;;; Nested contexts
(defcfun-debug-logging context-new-child-context context
  (parent-ctxt context))

(defcfun-debug-logging context-dump-reproducer-to-file :void
  (ctxt context)
  (path :string))

(defcfun-debug-logging context-enable-dump :void
  (ctxt context)
  (dumpname :string)
  (out-ptr (:pointer :string)))

;;; Timing support
;#+timing-api
(defctype timer :pointer)

(defcfun-debug-logging timer-new timer)

(defcfun-debug-logging timer-release timer
  (timer timer))

(defcfun-debug-logging context-set-timer :void
  (ctxt context)
  (timer timer))

(defcfun-debug-logging context-get-timer timer
  (ctxt context))

(defcfun-debug-logging timer-push :void
  (timer timer)
  (item-name :string))

(defcfun-debug-logging timer-pop :void
  (timer timer)
  (item-name :string))

(defcfun-debug-logging timer-print :void
  (timer timer)
  (f-out :pointer))                     ; FILE *

;#+rvalue-set-bool-require-tail-call
(defcfun-debug-logging rvalue-set-bool-require-tail-call :void
  (call rvalue)
  (require-tail-call :int))

;#+type-get-aligned
(defcfun-debug-logging type-get-aligned type
  (type type)
  (alignment-in-bytes :size))

;#+type-get-vector
(defcfun-debug-logging type-get-vector type
  (type type)
  (num-units :size))

;#+function-get-address
(defcfun-debug-logging function-get-address rvalue
  (fn function)
  (loc location))

;#+context-new-rvalue-from-vector
(defcfun-debug-logging context-new-rvalue-from-vector rvalue
  (ctxt context)
  (loc location)
  (vec-type type)
  (num-elements :size)
  (elements rvalue))

;#+version
(defcfun-debug-logging version-major :int)

(defcfun-debug-logging version-minor :int)

(defcfun-debug-logging version-patchlevel :int)

;;; Asm support
;#+asm-statements
(defcfun-debug-logging block-add-extended-asm extended-asm
  (block_ block)
  (loc location)
  (asm-template :string))

(defcfun-debug-logging block-end-with-extended-asm-goto  extended-asm
  (block_ block)
  (loc location)
  (asm-template :string)
  (num-goto-blocks :int)
  (goto-blocks (:pointer block))
  (fallthrough-block block))

(defcfun-debug-logging extended-asm-as-object object
  (ext-asm extended-asm))

(defcfun-debug-logging extended-asm-set-volatile-flag :void
  (ext-asm extended-asm)
  (flag :int))

(defcfun-debug-logging extended-asm-set-inline-flag :void
  (ext-asm extended-asm)
  (flag :int))

(defcfun-debug-logging extended-asm-add-output-operand :void
  (ext-asm extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (dest lvalue))

(defcfun-debug-logging extended-asm-add-input-operand :void
  (ext-asm extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (src rvalue))

(defcfun-debug-logging extended-asm-add-clobber :void
  (ext-asm extended-asm)
  (victim :string))

(defcfun-debug-logging add-top-level-asm :void
  (ctxt context)
  (loc location)
  (asm-stmts :string))

;#+reflection
(defcfun-debug-logging function-get-return-type type
  (func function))

(defcfun-debug-logging function-get-param-count :size
  (func function))

(defcfun-debug-logging type-dyncast-array type
  (type type))

(defcfun-debug-logging type-is-bool :int
  (type type))

(defcfun-debug-logging type-dyncast-function-ptr-type function-type
  (type type))

(defcfun-debug-logging function-type-get-return-type type
  (function-type function-type))

(defcfun-debug-logging function-type-get-param-count :size
  (function-type function-type))

(defcfun-debug-logging function-type-get-param-type type
  (function-type function-type)
  (index :size))

(defcfun-debug-logging type-is-integral :int
  (type type))

(defcfun-debug-logging type-is-pointer type
  (type type))

(defcfun-debug-logging type-dyncast-vector vector-type
  (type type))

(defcfun-debug-logging type-is-struct struct
  (type type))

(defcfun-debug-logging vector-type-get-num-units :size
  (vector-type vector-type))

(defcfun-debug-logging vector-type-get-element-type type
  (vector-type vector-type))

(defcfun-debug-logging type-unqualified type
  (type type))

(defcenum fn-attribute
  :fn-attribute-alias
  :fn-attribute-always-inline
  :fn-attribute-inline
  :fn-attribute-noinline
  :fn-attribute-target
  :fn-attribute-used
  :fn-attribute-visibility
  :fn-attribute-cold
  :fn-attribute-returns-twice
  :fn-attribute-pure
  :fn-attribute-const
  :fn-attribute-weak
  :fn-attribute-nonnull
  :fn-attribute-max                                  ; just for indicating the maximum value of this enum
  )

(defcfun-debug-logging function-add-attribute :void
  (func function)
  (attribute fn-attribute))

(defcfun-debug-logging function-add-string-attribute :void
  (func function)
  (attribute fn-attribute)
  (value :string))

(defcfun-debug-logging function-add-integer-array-attribute :void
  (func function)
  (attribute fn-attribute)
  (value (:pointer :int))
  (length :size))

(defcenum variable-attribute
  :variable-attribute-visibility
  :variable-attribute-max)

(defcfun-debug-logging lvalue-add-string-attribute :void
  (variable lvalue)
  (attribute variable-attribute)
  (value :string))

;#+context-set-output-ident
(defcfun-debug-logging context-set-output-ident :void
  (ctxt context)
  (output-ident :string))
