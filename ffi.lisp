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
(defcfun context-acquire context)

(defcfun context-release :void
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

(defcfun context-set-str-option :void
  (ctxt context)
  (opt str-option)
  (value :string))

(defcfun context-set-int-option :void
  (ctxt context)
  (opt int-option)
  (value :int))

(defcfun context-set-bool-option :void
  (ctxt context)
  (opt bool-option)
  (value :int))

;#+allow-unreachable-blocks
(defcfun context-set-bool-allow-unreachable-blocks :void
  (ctxt context)
  (bool-value :int))

;#+print-errors-to-stderr
(defcfun context-set-bool-print-errors-to-stderr :void
  (ctxt context)
  (enabled :int))

;#+use-external-driver
(defcfun context-set-bool-use-external-driver :void
  (ctxt context)
  (bool-value :int))

;#+command-line-option
(defcfun context-add-command-line-option :void
  (ctxt context)
  (optname :string))

;#+driver-option
(defcfun context-add-driver-option :void
  (ctxt context)
  (optname :string))

(defcfun context-compile result
  (ctxt context))

(defcenum output-kind
  :output-kind-assmebler
  :output-kind-object-file
  :output-kind-dynamic-library
  :output-kind-executable)

(defcfun context-compile-to-file :void
  (ctxt context)
  (output-kind output-kind)
  (output-path :string))

(defcfun context-dump-to-file :void
  (ctxt context)
  (path :string)
  (update-locations :int))

(defcfun context-set-logfile :void
  (ctxt context)
  (logfile :pointer)
  (flags :int)
  (verbosity :int))

(defcfun context-get-first-error :string
  (ctxt context))

(defcfun context-get-last-error :string
  (ctxt context))

(defcfun result-get-code :pointer
  (result result)
  (funcname :string))

(defcfun result-get-global :pointer
  (result result)
  (name :string))

(defcfun result-release :void
  (result result))

;;; Objects
(defcfun object-get-context context
  (obj object))

(defcfun object-get-debug-string :string
  (obj object))

;;; Debugging information
(defcfun context-new-location location
  (ctxt context)
  (filename :string)
  (line :int)
  (column :int))

(defcfun location-as-object object
  (loc location))

;;; Types
(defcfun type-as-object object
  (type type))

(defcenum types
  :type-void                                 ; C's "void" type
  :type-void-ptr                             ; "void *"

  ;; C++'s bool type and C99 "_Bool" type, aka "bool" if using stdbool.h
  :type-bool

  ;; C's "char" and the variants
  :type-char
  :type-signed-char
  :type-unsigned-int-char

  ;; C's integer types
  :type-short
  :type-unsigned-int-short
  :type-int
  :type-unsigned-int-int
  :type-long
  :type-unsigned-int-long
  :type-long-long
  :type-unsigned-int-long-long

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

(defcfun context-get-type type
  (ctxt context)
  (type types))

(defcfun context-get-int-type type
  (ctxt context)
  (num-bytes :int)
  (is-signed :int))

(defcfun type-get-pointer type
  (type type))

(defcfun type-get-const type
  (type type))


(defcfun type-get-volatile type
  (type type))

;#+type-get-restrict
(defcfun type-get-restrict type
  (type type))

;#+sized-integers
(defcfun compatible-types :int
  (ltype type)
  (rtype type))

;#+sized-integers
(defcfun type-get-size :ssize
  (type type))

(defcfun context-new-array-type type
  (ctxt context)
  (loc location)
  (element-type type)
  (num-elements :int))

(defcfun context-new-field field
  (ctxt context)
  (loc location)
  (type type)
  (name :string))

;#+context-new-bitfield
(defcfun context-new-bitfield field
  (ctxt context)
  (loc location)
  (type type)
  (width :int)
  (name :string))

(defcfun field-as-object object
  (field field))

(defcfun context-new-struct-type struct
  (ctxt context)
  (loc location)
  (name :string)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun context-new-opaque-struct struct
  (ctxt context)
  (loc location)
  (name :string))

(defcfun struct-as-type type
  (struct-type struct))

(defcfun struct-set-fields struct
  (struct-type struct)
  (loc location)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun struct-get-field field
  (struct-type struct)
  (index :size))

(defcfun struct-get-field-count :size
  (struct-type struct))

(defcfun context-new-union-type type
  (ctxt context)
  (loc location)
  (name :string)
  (num-fields :int)
  (fields (:pointer field)))

(defcfun context-new-funciton-ptr-type type
  (ctxt context)
  (loc location)
  (return-type type)
  (num-params :int)
  (param-types (:pointer type))
  (is-variadic :int))

;;; Constructing functions
(defcfun context-new-param param
  (ctxt context)
  (loc location)
  (type type)
  (name :string))

(defcfun param-as-object object
  (param param))

(defcfun param-as-lvalue lvalue
  (param param))

(defcfun param-as-rvalue rvalue
  (param param))

(defcenum function-kind
  :function-kind-exported
  :function-kind-internel
  :function-kind-imported
  :function-kind-always-inline)

(defcenum tls-model
  :tls-model-none
  :tls-model-global-dynamic
  :tls-model-local-dynamic
  :tls-model-initial-exec
  :tls-model-local-exec)

(defcfun context-new-function function
  (ctxt context)
  (loc location)
  (kind function-kind)
  (return-type type)
  (name :string)
  (num-params :int)
  (params (:pointer param))
  (is-variadic :int))

(defcfun context-get-builtin-function function
  (ctxt context)
  (name :string))

(defcfun function-as-object object
  (func function))

(defcfun function-get-param param
  (func function)
  (index :int))

(defcfun function-dump-to-dot :void
  (func function)
  (path :string))

;;; Block
(defcfun function-new-block block
  (func function)
  (name :string))

(defcfun block-as-object object
  (block_ block))

(defcfun block-get-function function
  (block_ block))

;;; lvalues, rvalues and expressions
(defcenum global-kind
  :global-kind-exported
  :global-kind-internal
  :global-kind-imported)

(defcfun context-new-global lvalue
  (ctxt context)
  (loc location)
  (kind global-kind)
  (type type)
  (name :string))

;#+ctors
(defcfun context-new-struct-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (num-values :size)
  (fields (:pointer field))
  (values (:pointer rvalue)))

;#+ctors
(defcfun context-new-union-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (field field)
  (value rvalue))

;#+ctors
(defcfun context-new-array-constructor rvalue
  (ctxt context)
  (loc location)
  (type type)
  (num-values :size)
  (values (:pointer rvalue)))

;#+ctors
(defcfun global-set-initializer-rvalue lvalue
  (ctxt lvalue)
  (init-value rvalue))

;#+get-target-builtin-function
(defcfun context-get-target-builtin-funciton function
  (ctxt context)
  (name :string))

;#+global-set-initializer
(defcfun global-set-initializer lvalue
  (global lvalue)
  (blob :pointer)
  (num-bytes :size))

;#+global-set-readonly
(defcfun global-set-randomly :void
  (global lvalue))

(defcfun lvalue-as-object object
  (lvalue lvalue))

(defcfun lvalue-as-rvalue rvalue
  (lvalue lvalue))

(defcfun rvalue-as-object object
  (rvalue rvalue))

(defcfun rvalue-get-type type
  (rvalue rvalue))

(defcfun context-new-rvalue-from-int rvalue
  (ctxt context)
  (numeric-type type)
  (value :int))

(defcfun context-new-rvalue-from-long rvalue
  (ctxt context)
  (numeric-type type)
  (value :long))

(defcfun context-zero rvalue
  (ctxt context)
  (numeric-type type))

(defcfun context-one rvalue
  (ctxt context)
  (numeric-type type))

(defcfun context-new-rvalue-from-double rvalue
  (ctxt context)
  (numeric-type type)
  (value :double))

(defcfun context-new-rvalue-from-ptr rvalue
  (ctxt context)
  (pointer-type type)
  (value :pointer))

(defcfun context-null rvalue
  (ctxt context))

;#+context-new-sizeof
(defcfun context-new-sizeof rvalue
  (ctxt context)
  (type type))

;#+context-new-alignof
(defcfun context-new-alignof rvalue
  (ctxt context)
  (type type))

(defcfun context-new-string-literal rvalue
  (ctxt context)
  (value :string))

(defcenum unary-op
  :unary-op-minus
  :unary-op-bitwise-negate
  :unary-op-logical-negate
  :unary-op-abs)

(defcfun context-new-unary-op rvalue
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
  :binary-op-lgoical-or
  :binary-op-lshift
  :binary-op-rshift)

(defcfun context-new-binary-op rvalue
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

(defcfun context-new-comparison rvalue
  (ctxt context)
  (loc location)
  (op comparison)
  (a rvalue)
  (b rvalue))

(defcfun context-new-call rvalue
  (ctxt context)
  (loc location)
  (fn-ptr rvalue)
  (numargs :int)
  (args (:pointer rvalue)))

(defcfun context-new-call-through-ptr rvalue
  (ctxt context)
  (loc location)
  (fn-ptr rvalue)
  (numargs :int)
  (args (:pointer rvalue)))

(defcfun context-new-cast rvalue
  (ctxt context)
  (loc location)
  (value rvalue)
  (type type))

;#+context-new-bitcast
(defcfun context-new-bitcast rvalue
  (ctxt context)
  (loc location)
  (value rvalue)
  (type type))

;;#+alignment
(defcfun lvalue-set-alignment :void
  (lvalue lvalue)
  (bytes :unsigned-int))

(defcfun lvalue-get-alignment :unsigned-int
  (lvalue lvalue))

(defcfun context-new-array-access lvalue
  (ctxt context)
  (loc location)
  (ptr rvalue)
  (index rvalue))

;#+context-convert-vector
(defcfun context-convert-vector rvalue
  (ctxt context)
  (loc location)
  (vector rvalue)
  (type type))

;#+vector-operations
(defcfun context-new-rvalue-vector-perm rvalue
  (ctxt context)
  (loc location)
  (elements1 rvalue)
  (elements2 rvalue)
  (mask rvalue))

;#+vector-operations
(defcfun context-new-vector-access lvalue
  (ctxt context)
  (loc location)
  (vector rvalue)
  (index rvalue))

(defcfun lvalue-access-field lvalue
  (struct-or-union lvalue)
  (loc location)
  (field field))

(defcfun rvalue-access-field rvalue
  (struct-or-union rvalue)
  (loc location)
  (field field))

(defcfun rvalue-dereference-field lvalue
  (ptr rvalue)
  (loc location)
  (field field))

(defcfun rvalue-dereference lvalue
  (rvalue rvalue)
  (loc location))

(defcfun lvalue-get-address rvalue
  (lvalue lvalue)
  (loc location))

;#+lvalue-set-tls-model
(defcfun lvalue-set-tls-model :void
  (lvalue lvalue)
  (model tls-model))

;#+lvalue-set-link-section
(defcfun lvalue-set-link-section :void
  (lvalue lvalue)
  (section-name :string))

;#+lvalue-set-register-name
(defcfun lvalue-set-register-name :void
  (lvalue lvalue)
  (reg-name :string))

(defcfun function-new-local lvalue
  (func function)
  (loc location)
  (type type)
  (name :string))

(defcfun function-new-temp lvalue
  (func function)
  (loc location)
  (type type))

;;; Statement creation
(defcfun block-add-eval :void
  (block_ block)
  (loc location)
  (rvalue rvalue))

(defcfun block-add-assignment :void
  (block_ block)
  (loc location)
  (lvalue lvalue)
  (rvalue rvalue))

(defcfun block-add-assignment-op :void
  (block_ block)
  (loc location)
  (lvalue lvalue)
  (op binary-op)
  (rvalue rvalue))

(defcfun block-add-comment :void
  (block_ block)
  (loc location)
  (text :string))

(defcfun block-end-with-conditional :void
  (block_ block)
  (loc location)
  (boolval rvalue)
  (on-true block)
  (on-false block))

(defcfun block-end-with-jump :void
  (block_ block)
  (loc location)
  (target block))

(defcfun block-end-with-return :void
  (block_ block)
  (loc location)
  (rvalue rvalue))

(defcfun block-end-with-void-return :void
  (block_ block)
  (loc location))

;#+switch-statements
(defcfun context-new-case case
  (ctxt context)
  (min-value rvalue)
  (max-value rvalue)
  (dest-block block))

(defcfun case-as-object object
  (case_ case))

(defcfun block-end-with-switch :void
  (block_ block)
  (loc location)
  (expr rvalue)
  (default-block block)
  (num-cases :int)
  (cases (:pointer case)))

;;; Nested contexts
(defcfun context-new-child-context context
  (parent-ctxt context))

(defcfun context-dump-reproducer-to-file :void
  (ctxt context)
  (path :string))

(defcfun context-enable-dump :void
  (ctxt context)
  (dumpname :string)
  (out-ptr (:pointer :string)))

;;; Timing support
;#+timing-api
(defctype timer :pointer)

(defcfun timer-new timer)

(defcfun timer-release timer
  (timer timer))

(defcfun context-set-timer :void
  (ctxt context)
  (timer timer))

(defcfun context-get-timer timer
  (ctxt context))

(defcfun timer-push :void
  (timer timer)
  (item-name :string))

(defcfun timer-pop :void
  (timer timer)
  (item-name :string))

(defcfun timer-print :void
  (timer timer)
  (f-out :pointer))                     ; FILE *

;#+rvalue-set-bool-require-tail-call
(defcfun rvalue-set-bool-require-tail-call :void
  (call rvalue)
  (require-tail-call :int))

;#+type-get-aligned
(defcfun type-get-aligned type
  (type type)
  (alignment-in-bytes :size))

;#+type-get-vector
(defcfun type-get-vector type
  (type type)
  (num-units :size))

;#+function-get-address
(defcfun function-get-address rvalue
  (fn function)
  (loc location))

;#+context-new-rvalue-from-vector
(defcfun context-new-rvalue-from-vector rvalue
  (ctxt context)
  (loc location)
  (vec-type type)
  (num-elements :size)
  (elements rvalue))

;#+version
(defcfun version-major :int)

(defcfun version-minor :int)

(defcfun version-patchlevel :int)

;;; Asm support
;#+asm-statements
(defcfun block-add-extended-asm extended-asm
  (block_ block)
  (loc location)
  (asm-template :string))

(defcfun block-end-with-extended-asm-goto  extended-asm
  (block_ block)
  (loc location)
  (asm-template :string)
  (num-goto-blocks :int)
  (goto-blocks (:pointer block))
  (fallthrough-block block))

(defcfun extended-asm-as-object object
  (ext-asm extended-asm))

(defcfun extended-asm-set-volatile-flag :void
  (ext-asm extended-asm)
  (flag :int))

(defcfun extended-asm-set-inline-flag :void
  (ext-asm extended-asm)
  (flag :int))

(defcfun extended-asm-add-output-operand :void
  (ext-asm extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (dest lvalue))

(defcfun extended-asm-add-input-operand :void
  (ext-asm extended-asm)
  (asm-symbolic-name :string)
  (constraint :string)
  (src rvalue))

(defcfun extended-asm-add-clobber :void
  (ext-asm extended-asm)
  (victim :string))

(defcfun add-top-level-asm :void
  (ctxt context)
  (loc location)
  (asm-stmts :string))

;#+reflection
(defcfun function-get-return-type type
  (func function))

(defcfun function-get-param-count :size
  (func function))

(defcfun type-dyncast-array type
  (type type))

(defcfun type-is-bool :int
  (type type))

(defcfun type-dyncast-function-ptr-type function-type
  (type type))

(defcfun function-type-get-return-type type
  (function-type function-type))

(defcfun function-type-get-param-count :size
  (function-type function-type))

(defcfun function-type-get-param-type type
  (function-type function-type)
  (index :size))

(defcfun type-is-integral :int
  (type type))

(defcfun type-is-pointer type
  (type type))

(defcfun type-dyncast-vector vector-type
  (type type))

(defcfun type-is-struct struct
  (type type))

(defcfun vector-type-get-num-units :size
  (vector-type vector-type))

(defcfun vector-type-get-element-type type
  (vector-type vector-type))

(defcfun type-unqualified type
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
  :fn-attribute-max                                  ; just for indicate the maximum value of this enum
  )

(defcfun funciton-add-attribute :void
  (func function)
  (attribute fn-attribute))

(defcfun function-add-string-attribute :void
  (func function)
  (attribute fn-attribute)
  (value :string))

(defcfun function-add-integer-array-attribute :void
  (func function)
  (attribute fn-attribute)
  (value (:pointer :int))
  (length :size))

(defcenum variable-attribute
  :variable-attribute-visibility
  :variable-attribute-max)

(defcfun lvalue-add-string-attribute :void
  (variable lvalue)
  (attribute variable-attribute)
  (value :string))

;#+context-set-output-ident
(defcfun context-set-output-ident :void
  (ctxt context)
  (output-ident :string))
