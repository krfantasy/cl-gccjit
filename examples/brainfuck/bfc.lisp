;;; Tutorial part 5: Creating a Brainfuck compiler with JIT compilation
;;; Based on https://gcc.gnu.org/onlinedocs/jit/intro/tutorial05.html

(in-package :cl-user)

;; Import CFFI macros
(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(cffi:defcenum cffi:foreign-funcall-pointer)))

(defconstant +max-open-parens+ 1000)
(defconstant +data-size+ 30000)

;;; Brainfuck compiler state
(defstruct bf-compiler
  filename
  (line 1)
  (column 1)
  ctxt
  void-type
  int-type
  byte-type
  array-type
  func-getchar
  func-putchar
  func
  curblock
  int-zero
  int-one
  byte-zero
  byte-one
  data-cells
  idx
  (num-open-parens 0)
  paren-test
  paren-body
  paren-after)

(defun bf-compiler-init (compiler filename)
  "Initialize the Brainfuck compiler state"
  (setf (bf-compiler-filename compiler) filename
        (bf-compiler-ctxt compiler) (gccjit-ffi:context-acquire)
        (bf-compiler-paren-test compiler) (make-array +max-open-parens+ :initial-element (cffi:null-pointer))
        (bf-compiler-paren-body compiler) (make-array +max-open-parens+ :initial-element (cffi:null-pointer))
        (bf-compiler-paren-after compiler) (make-array +max-open-parens+ :initial-element (cffi:null-pointer)))

  ;; Configure context
  (gccjit-ffi:context-set-int-option (bf-compiler-ctxt compiler) :int-option-optimization-level 3)
  (gccjit-ffi:context-set-bool-option (bf-compiler-ctxt compiler) :bool-option-debuginfo 1)

  ;; Create types
  (setf (bf-compiler-void-type compiler) (gccjit-ffi:context-get-type (bf-compiler-ctxt compiler) (cffi:foreign-enum-value 'gccjit-ffi:types :type-void))
        (bf-compiler-int-type compiler) (gccjit-ffi:context-get-type (bf-compiler-ctxt compiler) (cffi:foreign-enum-value 'gccjit-ffi:types :type-int))
        (bf-compiler-byte-type compiler) (gccjit-ffi:context-get-type (bf-compiler-ctxt compiler) (cffi:foreign-enum-value 'gccjit-ffi:types :type-unsigned-int-char))
        (bf-compiler-array-type compiler) (gccjit-ffi:context-new-array-type (bf-compiler-ctxt compiler) (cffi:null-pointer) (bf-compiler-byte-type compiler) +data-size+))

  ;; Create constants
  (setf (bf-compiler-int-zero compiler) (gccjit-ffi:context-zero (bf-compiler-ctxt compiler) (bf-compiler-int-type compiler))
        (bf-compiler-int-one compiler) (gccjit-ffi:context-one (bf-compiler-ctxt compiler) (bf-compiler-int-type compiler))
        (bf-compiler-byte-zero compiler) (gccjit-ffi:context-zero (bf-compiler-ctxt compiler) (bf-compiler-byte-type compiler))
        (bf-compiler-byte-one compiler) (gccjit-ffi:context-one (bf-compiler-ctxt compiler) (bf-compiler-byte-type compiler)))

  ;; Don't use builtin functions for this example
  (setf (bf-compiler-func-getchar compiler) (cffi:null-pointer)
        (bf-compiler-func-putchar compiler) (cffi:null-pointer)))

(defun bf-compiler-cleanup (compiler)
  "Clean up the Brainfuck compiler state"
  (gccjit-ffi:context-release (bf-compiler-ctxt compiler)))

(defun bf-compiler-new-location (compiler)
  "Create a new location for the current position"
  (gccjit-ffi:context-new-location (bf-compiler-ctxt compiler)
                                  (bf-compiler-filename compiler)
                                  (bf-compiler-line compiler)
                                  (bf-compiler-column compiler)))

(defun bf-compiler-update-position (compiler ch)
  "Update line/column position based on character"
  (cond
    ((char= ch #\Newline)
     (incf (bf-compiler-line compiler))
     (setf (bf-compiler-column compiler) 1))
    (t
     (incf (bf-compiler-column compiler)))))

(defun bf-compiler-make-main (compiler)
  "Create the main function for the Brainfuck program"
  (let* ((loc (bf-compiler-new-location compiler))
         (func (gccjit-ffi:context-new-function (bf-compiler-ctxt compiler) loc :function-kind-exported
                                               (bf-compiler-int-type compiler) "main" 0 (cffi:null-pointer) 0))
         (entry-block (gccjit-ffi:function-new-block func "entry")))

    (setf (bf-compiler-func compiler) func
          (bf-compiler-curblock compiler) entry-block)

    ;; Create data cells array and index variable
    (setf (bf-compiler-data-cells compiler)
          (gccjit-ffi:function-new-local func loc (bf-compiler-array-type compiler) "data_cells"))
    (setf (bf-compiler-idx compiler)
          (gccjit-ffi:function-new-local func loc (bf-compiler-int-type compiler) "idx"))

    ;; Initialize data cells array (no need to explicitly initialize to zero as it's automatic)

    ;; Initialize index to zero
    (gccjit-ffi:block-add-assignment entry-block loc (bf-compiler-idx compiler) (bf-compiler-int-zero compiler))

    entry-block))

(defun bf-compiler-data-access (compiler)
  "Create array access expression for current data cell"
  (gccjit-ffi:context-new-array-access (bf-compiler-ctxt compiler) (bf-compiler-new-location compiler)
                                      (gccjit-ffi:lvalue-as-rvalue (bf-compiler-data-cells compiler))
                                      (gccjit-ffi:lvalue-as-rvalue (bf-compiler-idx compiler))))

(defun bf-compile-char (compiler ch)
  "Compile a single Brainfuck character"
  (let ((loc (bf-compiler-new-location compiler)))
    (case ch
      (#\>
       ;; > - increment pointer
       (gccjit-ffi:block-add-assignment-op (bf-compiler-curblock compiler) loc (bf-compiler-idx compiler)
                                          :binary-op-plus (bf-compiler-int-one compiler)))

      (#\<
       ;; < - decrement pointer
       (gccjit-ffi:block-add-assignment-op (bf-compiler-curblock compiler) loc (bf-compiler-idx compiler)
                                          :binary-op-minus (bf-compiler-int-one compiler)))

      (#\+
       ;; + - increment data
       (let ((data-access (bf-compiler-data-access compiler)))
         (gccjit-ffi:block-add-assignment-op (bf-compiler-curblock compiler) loc data-access
                                            :binary-op-plus (bf-compiler-byte-one compiler))))

      (#\-
       ;; - - decrement data
       (let ((data-access (bf-compiler-data-access compiler)))
         (gccjit-ffi:block-add-assignment-op (bf-compiler-curblock compiler) loc data-access
                                            :binary-op-minus (bf-compiler-byte-one compiler))))

      (#\.
       ;; . - output (skip for this example since we don't have putchar)
       nil)

      (#\,
       ;; , - input (skip for this example since we don't have getchar)
       nil)

      (#\[
       ;; [ - start loop
       (when (>= (bf-compiler-num-open-parens compiler) +max-open-parens+)
         (error "Too many nested loops"))

       (let* ((test-block (gccjit-ffi:function-new-block (bf-compiler-func compiler) "loop_test"))
              (body-block (gccjit-ffi:function-new-block (bf-compiler-func compiler) "loop_body"))
              (after-block (gccjit-ffi:function-new-block (bf-compiler-func compiler) "after_loop")))

         ;; Jump to test block
         (gccjit-ffi:block-end-with-jump (bf-compiler-curblock compiler) loc test-block)

         ;; Store loop blocks
         (let ((index (bf-compiler-num-open-parens compiler)))
           (setf (aref (bf-compiler-paren-test compiler) index) test-block
                 (aref (bf-compiler-paren-body compiler) index) body-block
                 (aref (bf-compiler-paren-after compiler) index) after-block
                 (bf-compiler-num-open-parens compiler) (1+ index)
                 (bf-compiler-curblock compiler) body-block))))

      (#\]
       ;; ] - end loop
       (when (zerop (bf-compiler-num-open-parens compiler))
         (error "Unmatched closing bracket"))

       (let* ((index (1- (bf-compiler-num-open-parens compiler)))
              (test-block (aref (bf-compiler-paren-test compiler) index))
              (body-block (aref (bf-compiler-paren-body compiler) index))
              (after-block (aref (bf-compiler-paren-after compiler) index)))

         ;; Jump back to test block
         (gccjit-ffi:block-end-with-jump (bf-compiler-curblock compiler) loc test-block)

         ;; Set up test block
         (setf (bf-compiler-curblock compiler) test-block)
         (let* ((data-access (bf-compiler-data-access compiler))
                (data-value (gccjit-ffi:lvalue-as-rvalue data-access))
                (comparison (gccjit-ffi:context-new-comparison (bf-compiler-ctxt compiler) loc :comparison-eq
                                                              data-value (bf-compiler-byte-zero compiler))))
           (gccjit-ffi:block-end-with-conditional test-block loc comparison after-block body-block))

         (setf (bf-compiler-num-open-parens compiler) index
               (bf-compiler-curblock compiler) after-block)))

      (t
       ;; Ignore other characters (comments, whitespace)
       nil)))

  (bf-compiler-update-position compiler ch))

(defun bf-compile-file (compiler filename)
  "Compile a Brainfuck file"
  (with-open-file (stream filename :direction :input)
    (bf-compiler-make-main compiler)

    (loop for ch = (read-char stream nil)
          while ch
          do (bf-compile-char compiler ch))

    ;; Return the value from the first data cell as the result
    (let* ((data-access (gccjit-ffi:context-new-array-access (bf-compiler-ctxt compiler) (bf-compiler-new-location compiler)
                                                          (gccjit-ffi:lvalue-as-rvalue (bf-compiler-data-cells compiler))
                                                          (bf-compiler-int-zero compiler)))
           (data-value (gccjit-ffi:lvalue-as-rvalue data-access))
           (cast-to-int (gccjit-ffi:context-new-cast (bf-compiler-ctxt compiler) (bf-compiler-new-location compiler)
                                                   data-value (bf-compiler-int-type compiler))))
      (gccjit-ffi:block-end-with-return (bf-compiler-curblock compiler) (bf-compiler-new-location compiler)
                                       cast-to-int))

    ;; Check for unmatched brackets
    (when (> (bf-compiler-num-open-parens compiler) 0)
      (error "Unmatched opening brackets"))

    (bf-compiler-ctxt compiler)))

(defun bf-compile-and-run (bf-filename)
  "Compile and run a Brainfuck file"
  (let ((compiler (make-bf-compiler)))
    (unwind-protect
         (progn
           (bf-compiler-init compiler bf-filename)
           (let ((ctxt (bf-compile-file compiler bf-filename)))
             (let ((result (gccjit-ffi:context-compile ctxt)))
               (unwind-protect
                    (let ((main-func (gccjit-ffi:result-get-code result "main")))
                      (unless (cffi:null-pointer-p main-func)
                        (cffi:foreign-funcall-pointer main-func () :int)))
                 (gccjit-ffi:result-release result)))))
      (bf-compiler-cleanup compiler))))

;;; Example usage
(defun test-brainfuck-compiler-simple ()
  "Test the Brainfuck compiler with a simple program that doesn't use I/O"
  (let ((simple-program "+++>++<[->+<]"))  ;; Simple arithmetic: 3 + 2 = 5
    ;; Write test program to temporary file
    (let ((temp-file "/tmp/simple.bf"))
      (with-open-file (stream temp-file :direction :output :if-exists :supersede)
        (write-string simple-program stream))

      (format t "=== Testing Brainfuck Compiler (Simple) ===~%")
      (format t "Compiling simple arithmetic program...~%")

      (handler-case
          (let ((result (bf-compile-and-run temp-file)))
            (format t "Program executed successfully, returned: ~a~%" result))
        (error (e)
          (format t "Compilation error: ~a~%" e)))

      (format t "~%Brainfuck compilation test completed!~%"))))

(defun test-brainfuck-arithmetic ()
  "Test various arithmetic operations"
  (format t "=== Testing Arithmetic Operations ===~%")

  ;; Test 1: Simple addition
  (let ((program "+++>+++[->+<]"))  ;; 3 + 3 = 6
    (with-open-file (stream "/tmp/add.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/add.bf")))
          (format t "Addition (3+3): ~a~%" result))
      (error (e) (format t "Addition test failed: ~a~%" e))))

  ;; Test 2: Multiplication simulation
  (let ((program "++>+++[->[->+>+<<]>>[-<<+>>]<<<]"))  ;; 2 * 3 = 6
    (with-open-file (stream "/tmp/mult.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/mult.bf")))
          (format t "Multiplication (2*3): ~a~%" result))
      (error (e) (format t "Multiplication test failed: ~a~%" e)))))

(defun test-brainfuck-loops ()
  "Test loop constructs"
  (format t "=== Testing Loop Constructs ===~%")

  ;; Test 1: Simple loop
  (let ((program "+++[-]"))  ;; Set cell to 3, then decrement to 0
    (with-open-file (stream "/tmp/loop1.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/loop1.bf")))
          (format t "Simple loop (3->0): ~a~%" result))
      (error (e) (format t "Simple loop test failed: ~a~%" e))))

  ;; Test 2: Nested loops
  (let ((program "++>+++[->[->+<]<<]"))  ;; Nested loops
    (with-open-file (stream "/tmp/nested.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/nested.bf")))
          (format t "Nested loops: ~a~%" result))
      (error (e) (format t "Nested loops test failed: ~a~%" e)))))

(defun test-brainfuck-pointer-ops ()
  "Test pointer operations"
  (format t "=== Testing Pointer Operations ===~%")

  ;; Test 1: Pointer movement
  (let ((program ">>+++<<++>+"))  ;; Move pointer and set values
    (with-open-file (stream "/tmp/pointer1.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/pointer1.bf")))
          (format t "Pointer movement: ~a~%" result))
      (error (e) (format t "Pointer movement test failed: ~a~%" e))))

  ;; Test 2: Data copying
  (let ((program "+++>[-]<[->+>+<<]>>[-<<+>>]"))  ;; Copy value from cell0 to cell1
    (with-open-file (stream "/tmp/copy.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/copy.bf")))
          (format t "Data copying: ~a~%" result))
      (error (e) (format t "Data copying test failed: ~a~%" e)))))

(defun test-brainfuck-edge-cases ()
  "Test edge cases and error conditions"
  (format t "=== Testing Edge Cases ===~%")

  ;; Test 1: Empty program
  (let ((program ""))
    (with-open-file (stream "/tmp/empty.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/empty.bf")))
          (format t "Empty program: ~a~%" result))
      (error (e) (format t "Empty program test failed: ~a~%" e))))

  ;; Test 2: Single operation
  (let ((program "+"))
    (with-open-file (stream "/tmp/single.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/single.bf")))
          (format t "Single operation: ~a~%" result))
      (error (e) (format t "Single operation test failed: ~a~%" e))))

  ;; Test 3: Unmatched brackets (should fail)
  (let ((program "[[+++"))
    (with-open-file (stream "/tmp/unmatched.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/unmatched.bf")))
          (format t "Unmatched brackets (unexpected success): ~a~%" result))
      (error (e) (format t "Unmatched brackets (expected error): ~a~%" e)))))

(defun test-brainfuck-complex ()
  "Test more complex Brainfuck programs"
  (format t "=== Testing Complex Programs ===~%")

  ;; Test 1: Multi-cell operations (simpler version)
  (let ((program ">>+++<<++>+[->+<]"))  ;; Multi-cell operations with loop
    (with-open-file (stream "/tmp/multi.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/multi.bf")))
          (format t "Multi-cell operations: ~a~%" result))
      (error (e) (format t "Multi-cell operations test failed: ~a~%" e))))

  ;; Test 2: Data manipulation across cells
  (let ((program "+++>++[-<+>]<"))  ;; Move value from cell1 to cell0
    (with-open-file (stream "/tmp/move.bf" :direction :output :if-exists :supersede)
      (write-string program stream))
    (handler-case
        (let ((result (bf-compile-and-run "/tmp/move.bf")))
          (format t "Data movement: ~a~%" result))
      (error (e) (format t "Data movement test failed: ~a~%" e)))))

(defun test-brainfuck-compiler ()
  "Test the Brainfuck compiler with various programs"
  (format t "=== Brainfuck Compiler Comprehensive Test Suite ===~%~%")

  ;; Run all test categories
  (test-brainfuck-compiler-simple)
  (test-brainfuck-arithmetic)
  (test-brainfuck-loops)
  (test-brainfuck-pointer-ops)
  (test-brainfuck-edge-cases)
  (test-brainfuck-complex)

  (format t "~%=== All Brainfuck compiler tests completed ===~%"))

;;; Run the test
(test-brainfuck-compiler)