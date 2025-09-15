;;; Tutorial part 4: Creating a toy interpreter with JIT compilation
;;; Based on https://gcc.gnu.org/onlinedocs/jit/intro/tutorial04.html

(in-package :cl-user)

;; Import CFFI macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cffi:defcenum cffi:foreign-funcall-pointer)))

(defparameter *max-stack-depth* 8)
(defparameter *max-ops* 100)

;;; Opcode definitions
(cffi:defcenum opcode
  :dup
  :rot
  :binary-add
  :binary-subtract
  :binary-mult
  :binary-compare-lt
  :recurse
  :return
  :push-const
  :jump-abs-if-true)

;;; Operation structure
(defstruct toyvm-op
  (opcode :push-const)
  (operand 0)
  (linenum 0))

;;; Function structure
(defstruct toyvm-function
  (filename "unknown")
  (ops (make-array *max-ops* :element-type 'toyvm-op :initial-element (make-toyvm-op)))
  (num-ops 0))

;;; Compiled function result
(defstruct toyvm-compiled-function
  jit-result
  code)

;;; Compilation state
(defstruct compilation-state
  ctxt
  int-type
  bool-type
  stack-type
  const-one
  fn
  param-arg
  stack
  stack-depth
  x
  y
  op-locs
  initial-block
  op-blocks)

(defun add-push (state block rvalue loc)
  "Push a value onto the stack"
  (let* ((stack-array-access (gccjit-ffi:context-new-array-access
                              (compilation-state-ctxt state)
                              loc
                              (gccjit-ffi:lvalue-as-rvalue (compilation-state-stack state))
                              (gccjit-ffi:lvalue-as-rvalue (compilation-state-stack-depth state)))))
    (gccjit-ffi:block-add-assignment block loc stack-array-access rvalue)
    (gccjit-ffi:block-add-assignment-op block loc (compilation-state-stack-depth state)
                                       :binary-op-plus (compilation-state-const-one state))))

(defun add-pop (state block lvalue loc)
  "Pop a value from the stack"
  (gccjit-ffi:block-add-assignment-op block loc (compilation-state-stack-depth state)
                                     :binary-op-minus (compilation-state-const-one state))
  (let* ((stack-array-access (gccjit-ffi:context-new-array-access
                              (compilation-state-ctxt state)
                              loc
                              (gccjit-ffi:lvalue-as-rvalue (compilation-state-stack state))
                              (gccjit-ffi:lvalue-as-rvalue (compilation-state-stack-depth state)))))
    (gccjit-ffi:block-add-assignment block loc lvalue
                                    (gccjit-ffi:lvalue-as-rvalue stack-array-access))))

(defun compile-op (state block op loc next-block)
  "Compile a single opcode"
  (case (toyvm-op-opcode op)
    (:dup
     ;; X = pop(); push(X); push(X);
     (add-pop state block (compilation-state-x state) loc)
     (add-push state block (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state)) loc)
     (add-push state block (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state)) loc)
     (gccjit-ffi:block-end-with-jump block loc next-block))

    (:rot
     ;; Y = pop(); X = pop(); push(Y); push(X);
     (add-pop state block (compilation-state-y state) loc)
     (add-pop state block (compilation-state-x state) loc)
     (add-push state block (gccjit-ffi:lvalue-as-rvalue (compilation-state-y state)) loc)
     (add-push state block (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state)) loc)
     (gccjit-ffi:block-end-with-jump block loc next-block))

    (:binary-add
     ;; Y = pop(); X = pop(); push(X + Y);
     (add-pop state block (compilation-state-y state) loc)
     (add-pop state block (compilation-state-x state) loc)
     (let ((sum (gccjit-ffi:context-new-binary-op
                 (compilation-state-ctxt state) loc :binary-op-plus
                 (compilation-state-int-type state)
                 (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))
                 (gccjit-ffi:lvalue-as-rvalue (compilation-state-y state)))))
       (add-push state block sum loc)
       (gccjit-ffi:block-end-with-jump block loc next-block)))

    (:binary-subtract
     ;; Y = pop(); X = pop(); push(X - Y);
     (add-pop state block (compilation-state-y state) loc)
     (add-pop state block (compilation-state-x state) loc)
     (let ((diff (gccjit-ffi:context-new-binary-op
                  (compilation-state-ctxt state) loc :binary-op-minus
                  (compilation-state-int-type state)
                  (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))
                  (gccjit-ffi:lvalue-as-rvalue (compilation-state-y state)))))
       (add-push state block diff loc)
       (gccjit-ffi:block-end-with-jump block loc next-block)))

    (:binary-mult
     ;; Y = pop(); X = pop(); push(X * Y);
     (add-pop state block (compilation-state-y state) loc)
     (add-pop state block (compilation-state-x state) loc)
     (let ((product (gccjit-ffi:context-new-binary-op
                     (compilation-state-ctxt state) loc :binary-op-mult
                     (compilation-state-int-type state)
                     (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))
                     (gccjit-ffi:lvalue-as-rvalue (compilation-state-y state)))))
       (add-push state block product loc)
       (gccjit-ffi:block-end-with-jump block loc next-block)))

    (:binary-compare-lt
     ;; Y = pop(); X = pop(); push(X < Y);
     (add-pop state block (compilation-state-y state) loc)
     (add-pop state block (compilation-state-x state) loc)
     (let ((comparison (gccjit-ffi:context-new-cast
                        (compilation-state-ctxt state) loc
                        (gccjit-ffi:context-new-comparison
                         (compilation-state-ctxt state) loc :comparison-lt
                         (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))
                         (gccjit-ffi:lvalue-as-rvalue (compilation-state-y state)))
                        (compilation-state-int-type state))))
       (add-push state block comparison loc)
       (gccjit-ffi:block-end-with-jump block loc next-block)))

    (:recurse
     ;; X = pop(); push(fn(X));
     (add-pop state block (compilation-state-x state) loc)
     (cffi:with-foreign-object (args-ptr :pointer 1)
       (setf (cffi:mem-aref args-ptr :pointer 0)
             (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state)))
       (let ((call-result (gccjit-ffi:context-new-call
                           (compilation-state-ctxt state) loc
                           (gccjit-ffi:function-as-object (compilation-state-fn state))
                           1 args-ptr)))
         (add-push state block call-result loc)
         (gccjit-ffi:block-end-with-jump block loc next-block))))

    (:return
     ;; X = pop(); return X;
     (add-pop state block (compilation-state-x state) loc)
     (gccjit-ffi:block-end-with-return block loc
                                      (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))))

    (:push-const
     ;; push(constant);
     (let ((constant (gccjit-ffi:context-new-rvalue-from-int
                      (compilation-state-ctxt state)
                      (compilation-state-int-type state)
                      (toyvm-op-operand op))))
       (add-push state block constant loc)
       (gccjit-ffi:block-end-with-jump block loc next-block)))

    (:jump-abs-if-true
     ;; X = pop(); if (X) goto target;
     (add-pop state block (compilation-state-x state) loc)
     (let ((target-block (aref (compilation-state-op-blocks state) (toyvm-op-operand op))))
       (gccjit-ffi:block-end-with-conditional
        block loc
        (gccjit-ffi:context-new-comparison
         (compilation-state-ctxt state) loc :comparison-ne
         (gccjit-ffi:lvalue-as-rvalue (compilation-state-x state))
         (gccjit-ffi:context-zero (compilation-state-ctxt state) (compilation-state-int-type state)))
        target-block next-block)))

    (t (error "Unknown opcode: ~a" (toyvm-op-opcode op)))))

(defun toyvm-function-compile (fn)
  "Compile a toyvm function to native code"
  (let* ((ctxt (gccjit-ffi:context-acquire))
         (state (make-compilation-state :ctxt ctxt)))

    ;; Configure context
    (gccjit-ffi:context-set-bool-option ctxt :bool-option-debuginfo 1)

    ;; Create types
    (setf (compilation-state-int-type state)
          (gccjit-ffi:context-get-type ctxt (cffi:foreign-enum-value 'gccjit-ffi:types :type-int)))
    (setf (compilation-state-bool-type state)
          (gccjit-ffi:context-get-type ctxt (cffi:foreign-enum-value 'gccjit-ffi:types :type-bool)))
    (setf (compilation-state-stack-type state)
          (gccjit-ffi:context-new-array-type ctxt (cffi:null-pointer)
                                            (compilation-state-int-type state)
                                            *max-stack-depth*))
    (setf (compilation-state-const-one state)
          (gccjit-ffi:context-one ctxt (compilation-state-int-type state)))

    ;; Create source locations for each operation
    (setf (compilation-state-op-locs state)
          (make-array (toyvm-function-num-ops fn)))
    (dotimes (pc (toyvm-function-num-ops fn))
      (setf (aref (compilation-state-op-locs state) pc)
            (gccjit-ffi:context-new-location
             ctxt (toyvm-function-filename fn)
             (toyvm-op-linenum (aref (toyvm-function-ops fn) pc)) 0)))

    ;; Create function parameter and function
    (setf (compilation-state-param-arg state)
          (gccjit-ffi:context-new-param ctxt (aref (compilation-state-op-locs state) 0)
                                       (compilation-state-int-type state) "arg"))

    (cffi:with-foreign-object (params-ptr :pointer 1)
      (setf (cffi:mem-aref params-ptr :pointer 0) (compilation-state-param-arg state))
      (setf (compilation-state-fn state)
            (gccjit-ffi:context-new-function
             ctxt (aref (compilation-state-op-locs state) 0)
             :function-kind-exported
             (compilation-state-int-type state)
             "compiled_fn" 1 params-ptr 0)))

    ;; Create local variables
    (setf (compilation-state-stack state)
          (gccjit-ffi:function-new-local (compilation-state-fn state) (cffi:null-pointer)
                                        (compilation-state-stack-type state) "stack"))
    (setf (compilation-state-stack-depth state)
          (gccjit-ffi:function-new-local (compilation-state-fn state) (cffi:null-pointer)
                                        (compilation-state-int-type state) "stack-depth"))
    (setf (compilation-state-x state)
          (gccjit-ffi:function-new-local (compilation-state-fn state) (cffi:null-pointer)
                                        (compilation-state-int-type state) "x"))
    (setf (compilation-state-y state)
          (gccjit-ffi:function-new-local (compilation-state-fn state) (cffi:null-pointer)
                                        (compilation-state-int-type state) "y"))

    ;; Create blocks for each operation
    (setf (compilation-state-initial-block state)
          (gccjit-ffi:function-new-block (compilation-state-fn state) "initial"))
    (setf (compilation-state-op-blocks state)
          (make-array (toyvm-function-num-ops fn)))
    (dotimes (pc (toyvm-function-num-ops fn))
      (setf (aref (compilation-state-op-blocks state) pc)
            (gccjit-ffi:function-new-block
             (compilation-state-fn state)
             (format nil "instr~a" pc))))

    ;; Set up initial block
    (gccjit-ffi:block-add-assignment
     (compilation-state-initial-block state)
     (aref (compilation-state-op-locs state) 0)
     (compilation-state-stack-depth state)
     (gccjit-ffi:context-zero ctxt (compilation-state-int-type state)))

    (add-push state (compilation-state-initial-block state)
             (gccjit-ffi:param-as-rvalue (compilation-state-param-arg state))
             (aref (compilation-state-op-locs state) 0))

    (gccjit-ffi:block-end-with-jump
     (compilation-state-initial-block state)
     (aref (compilation-state-op-locs state) 0)
     (aref (compilation-state-op-blocks state) 0))

    ;; Compile each operation
    (dotimes (pc (toyvm-function-num-ops fn))
      (let ((next-block (if (< pc (1- (toyvm-function-num-ops fn)))
                           (aref (compilation-state-op-blocks state) (1+ pc))
                           (cffi:null-pointer))))
        (compile-op state
                   (aref (compilation-state-op-blocks state) pc)
                   (aref (toyvm-function-ops fn) pc)
                   (aref (compilation-state-op-locs state) pc)
                   next-block)))

    ;; Compile and return result
    (let* ((result (gccjit-ffi:context-compile ctxt))
           (fn-ptr (gccjit-ffi:result-get-code result "compiled_fn"))
           (cf (make-toyvm-compiled-function :jit-result result :code fn-ptr)))
      (gccjit-ffi:context-release ctxt)
      cf)))

;;; Example usage: Fibonacci function
(defun create-fibonacci-function ()
  "Create a toyvm function that computes Fibonacci numbers"
  (let ((fn (make-toyvm-function :filename "fibonacci" :num-ops 14)))
    ;; Fibonacci implementation using the toyvm opcodes
    (setf (aref (toyvm-function-ops fn) 0) (make-toyvm-op :opcode :push-const :operand 2 :linenum 1))
    (setf (aref (toyvm-function-ops fn) 1) (make-toyvm-op :opcode :binary-compare-lt :operand 0 :linenum 2))
    (setf (aref (toyvm-function-ops fn) 2) (make-toyvm-op :opcode :jump-abs-if-true :operand 13 :linenum 3))
    (setf (aref (toyvm-function-ops fn) 3) (make-toyvm-op :opcode :dup :operand 0 :linenum 4))
    (setf (aref (toyvm-function-ops fn) 4) (make-toyvm-op :opcode :push-const :operand 1 :linenum 5))
    (setf (aref (toyvm-function-ops fn) 5) (make-toyvm-op :opcode :binary-subtract :operand 0 :linenum 6))
    (setf (aref (toyvm-function-ops fn) 6) (make-toyvm-op :opcode :recurse :operand 0 :linenum 7))
    (setf (aref (toyvm-function-ops fn) 7) (make-toyvm-op :opcode :rot :operand 0 :linenum 8))
    (setf (aref (toyvm-function-ops fn) 8) (make-toyvm-op :opcode :push-const :operand 2 :linenum 9))
    (setf (aref (toyvm-function-ops fn) 9) (make-toyvm-op :opcode :binary-subtract :operand 0 :linenum 10))
    (setf (aref (toyvm-function-ops fn) 10) (make-toyvm-op :opcode :recurse :operand 0 :linenum 11))
    (setf (aref (toyvm-function-ops fn) 11) (make-toyvm-op :opcode :binary-add :operand 0 :linenum 12))
    (setf (aref (toyvm-function-ops fn) 12) (make-toyvm-op :opcode :return :operand 0 :linenum 13))
    ;; Base case: return the input value (for n < 2)
    (setf (aref (toyvm-function-ops fn) 13) (make-toyvm-op :opcode :return :operand 0 :linenum 14))
    fn))

;;; Additional test functions
(defun create-simple-arithmetic-function ()
  "Create a toyvm function that tests basic arithmetic"
  (let ((fn (make-toyvm-function :filename "arithmetic" :num-ops 8)))
    ;; (x + 5) * (x - 3)
    (setf (aref (toyvm-function-ops fn) 0) (make-toyvm-op :opcode :dup :operand 0 :linenum 1))
    (setf (aref (toyvm-function-ops fn) 1) (make-toyvm-op :opcode :push-const :operand 5 :linenum 2))
    (setf (aref (toyvm-function-ops fn) 2) (make-toyvm-op :opcode :binary-add :operand 0 :linenum 3))
    (setf (aref (toyvm-function-ops fn) 3) (make-toyvm-op :opcode :rot :operand 0 :linenum 4))
    (setf (aref (toyvm-function-ops fn) 4) (make-toyvm-op :opcode :push-const :operand 3 :linenum 5))
    (setf (aref (toyvm-function-ops fn) 5) (make-toyvm-op :opcode :binary-subtract :operand 0 :linenum 6))
    (setf (aref (toyvm-function-ops fn) 6) (make-toyvm-op :opcode :binary-mult :operand 0 :linenum 7))
    (setf (aref (toyvm-function-ops fn) 7) (make-toyvm-op :opcode :return :operand 0 :linenum 8))
    fn))

;;; Test the interpreter
(defun test-interpreter ()
  "Test the toy interpreter with multiple functions"
  (format t "=== Testing Fibonacci Function ===~%")
  (let* ((fib-fn (create-fibonacci-function))
         (compiled (toyvm-function-compile fib-fn)))
    (format t "Fibonacci sequence:~%")
    (dotimes (n 10)
      (let ((result (cffi:foreign-funcall-pointer (toyvm-compiled-function-code compiled) () :int n :int)))
        (format t "fib(~a) = ~a~%" n result)))
    (gccjit-ffi:result-release (toyvm-compiled-function-jit-result compiled)))

  (format t "~%=== Testing Arithmetic Function ===~%")
  (let* ((arith-fn (create-simple-arithmetic-function))
         (compiled (toyvm-function-compile arith-fn)))
    (format t "(x + 5) * (x - 3):~%")
    (dotimes (n 6)
      (let ((result (cffi:foreign-funcall-pointer (toyvm-compiled-function-code compiled) () :int n :int)))
        (format t "f(~a) = ~a~%" n result)))
    (gccjit-ffi:result-release (toyvm-compiled-function-jit-result compiled)))

  (format t "~%All tests completed successfully!~%"))

;;; Run the test
;(test-interpreter)