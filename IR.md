# High-Level IR Language Syntax

This document describes the syntax for the high-level Intermediate Representation (IR) language for `cl-gccjit`. The IR is based on S-expressions and is designed to be simple, expressive, and well-integrated with Common Lisp.

## Core Concepts

The IR language is built around a few core concepts:
1. **Functions** - Named blocks of code with parameters and return types
2. **Blocks** - Basic blocks of code with a single entry and exit point
3. **Variables** - Named storage locations with types
4. **Expressions** - Computations that produce values
5. **Statements** - Actions that modify state or control flow

## Syntax Overview

All IR constructs are expressed as S-expressions (lists). The general form is:

```
(keyword arg1 arg2 ... argN)
```

Keywords are special identifiers that define the type of construct. Arguments can be literals, variables, or nested expressions.

## Functions

Functions are the top-level constructs in the IR. They have a name, return type, parameters, and a body consisting of blocks.

### Syntax

```
(defun name (param1 param2 ... paramN) -> return-type
  block1
  block2
  ...
  blockN)
```

### Example

```
(defun square (x) -> :int
  (block entry
    (return (* x x))))
```

## Blocks

Blocks are sequences of statements that end with a control flow instruction. Each block has a unique name within its function.

### Syntax

```
(block name
  statement1
  statement2
  ...
  control-flow-statement)
```

### Example

```
(block loop
  (set i (+ i 1))
  (jump check))

(block check
  (if (< i 10)
    (jump loop)
    (jump end)))
```

## Global Variables

Global variables exist for the entire lifetime of the program.

### Syntax

Simple form:
```
(global name initial-value)
```
- `name`: The name of the global variable.
- `initial-value`: An expression for the initial value. The type of the variable is inferred from this value.

Extended form:
```
(global (name :kind kind) initial-value)
```
- `name`: The name of the variable.
- `:kind kind`: The linkage kind. Can be `:global-kind-exported`, `:global-kind-internal`, or `:global-kind-imported`. Defaults to `:global-kind-exported`.
- `initial-value`: An expression for the initial value.

*(Note: Thread-local storage (`:tls`) is not yet supported in this IR syntax.)*

### Example
```
(global my-global-var 42)

(global (another-global :kind :global-kind-internal) 100)
```

## Variables

Variables are declared with the `var` keyword and can have explicit types or have their types inferred.

### Syntax

```
(var name type initial-value)
(var name initial-value)  ; type inferred
```

### Examples

```
(var x :int 5)
(var y 10)  ; type inferred as int
(var message :string "Hello, world!")
```

## Assignments

Variables can be assigned new values using the `set` keyword.

### Syntax

```
(set variable-name new-value)
```

### Example

```
(var counter :int 0)
(set counter (+ counter 1))
```

## Expressions

Expressions compute values and can be nested. The IR supports arithmetic, comparison, and function call expressions.

### Arithmetic Expressions

```
(+ a b)     ; addition
(- a b)     ; subtraction
(* a b)     ; multiplication
(/ a b)     ; division
(% a b)     ; modulo
```

### Comparison Expressions

```
(= a b)     ; equality
(!= a b)    ; inequality
(< a b)     ; less than
(<= a b)    ; less than or equal
(> a b)     ; greater than
(>= a b)    ; greater than or equal
```

### Logical Expressions

```
(and a b)   ; logical and
(or a b)    ; logical or
(not a)     ; logical not
```

## Control Flow

The IR provides explicit control flow constructs based on blocks and jumps.

### Conditional Branching

```
(if condition
  (jump true-block)
  (jump false-block))
```

### Unconditional Jumps

```
(jump target-block)
```

### Switch Statements

The `switch` statement provides a multi-way branch based on the value of an expression. It is a more powerful alternative to a series of `if` statements.

#### Syntax

```
(switch expression
  (case (min-value max-value)
    (jump target-block))
  (case value
    (jump target-block))
  ...
  (default
    (jump default-target-block)))
```

- `expression`: An expression that evaluates to an integer value.
- `(case (min-value max-value) (jump target-block))`: If the value of `expression` is between `min-value` and `max-value` (inclusive), control is transferred to `target-block`.
- `(case value (jump target-block))`: A shorthand for `(case (value value) (jump target-block))`. If the value of `expression` is equal to `value`, control is transferred to `target-block`.
- `(default (jump default-target-block))`: If the value of `expression` does not match any of the `case` clauses, control is transferred to `default-target-block`.

#### Example

```
(defun day-of-week-name (day-num) -> :string
  (block entry
    (switch day-num
      (case 1 (jump monday))
      (case 2 (jump tuesday))
      (case 3 (jump wednesday))
      (case 4 (jump thursday))
      (case 5 (jump friday))
      (case (6 7) (jump weekend))
      (default (jump invalid-day)))
    ;; The switch statement must be the last thing in a block.
    )

  (block monday (return "Monday"))
  (block tuesday (return "Tuesday"))
  (block wednesday (return "Wednesday"))
  (block thursday (return "Thursday"))
  (block friday (return "Friday"))
  (block weekend (return "Weekend"))
  (block invalid-day (return "Invalid day")))
```

### Return Statements

```
(return value)
(return)  ; for void functions
```

## Function Calls

Functions can be called with the function name followed by arguments.

### Syntax

```
(function-name arg1 arg2 ... argN)
```

### Example

```
(var result (square 5))
    (print "Result: " result)
```

## Type Casting

The IR provides a `cast` expression to explicitly convert a value from one type to another.

### Syntax

```
(cast expression to-type)
```

- `expression`: The value to be converted (can be a variable or another expression).
- `to-type`: The target type to cast the expression to.

### Example

This example shows casting a float to an int.

```
(var float-value :float 3.14)
(var int-value :int (cast float-value :int)) ; int-value will be 3
```

Another example, casting an integer to a float.
```
(var i :int 10)
(var f :float (cast i :float)) ; f will be 10.0
```

## Pointers and Memory

The IR provides constructs for working with pointers, allowing for referencing and dereferencing memory.

### Referencing (`ref`)

The `ref` expression returns the memory address of an lvalue.

#### Syntax

```
(ref lvalue)
```

- `lvalue`: A variable or other modifiable location in memory.

#### Example

```
(var x :int 10)
(var x-ptr (ref x)) ; x-ptr now holds the address of x
```

### Dereferencing (`deref`)

The `deref` expression accesses the value at a given memory address. It can be used on the left-hand side of a `set` to modify the value at that address.

#### Syntax

```
(deref rvalue)
```

- `rvalue`: An expression that evaluates to a memory address (a pointer).

#### Example

```
;; Get the value from a pointer
(var value (deref x-ptr)) ; value will be 10

;; Modify the value using a pointer
(set (deref x-ptr) 20) ; the value of x is now 20
```

## Arrays

The IR supports array types and accessing array elements.

### Array Types

An array type is defined by its element type and size.

#### Syntax
```
(array-type element-type size)
```

#### Example
```
;; A variable holding an array of 10 integers.
(var my-array (array-type :int 10))
```

### Array Element Access (`at`)

The `at` expression is used to access an element of an array. It results in an lvalue, meaning it can be used for both reading and writing.

#### Syntax
```
(at array-expression index-expression)
```
- `array-expression`: An expression that evaluates to a pointer to the first element of the array. This is typically a variable of an array type.
- `index-expression`: An expression that evaluates to an integer index.

#### Example
```
(var my-array (array-type :int 10))

;; Set the value of the first element
(set (at my-array 0) 42)

;; Get the value of the first element
(var first-element (at my-array 0))
```

## Structs

The IR allows defining new structure types.

### Syntax
```
(struct name
  (field1-name field1-type)
  (field2-name field2-type)
  ...)
```
- `name`: The name of the new struct type.
- `(field-name field-type)`: A list of fields, each with a name and a type.

### Example
```
(struct point
  (x :int)
  (y :int))

(var p1 point)
```

## Struct Field Access

The IR provides a way to access fields of a struct using the `field` expression.

### Syntax
```
(field struct-expr field-name)
```
- `struct-expr`: An expression that evaluates to a struct value.
- `field-name`: The name of the field to access.

This expression returns the value of the specified field as an rvalue.

### Example
```
(struct point
  (x :int)
  (y :int))

(var p1 point)
(set (field p1 x) 10)
(set (field p1 y) 20)
(var x-value (field p1 x))  ; x-value will be 10
```

## Type Information

The IR provides expressions to get information about types, such as their size and alignment.

### Size of Type (`sizeof`)

The `sizeof` expression returns the size of a given type in bytes.

#### Syntax

```
(sizeof type)
```

- `type`: The type to get the size of.

#### Example

```
(var size-of-int (sizeof :int)) ; returns the size of an integer
```

### Alignment of Type (`alignof`)

The `alignof` expression returns the alignment of a given type in bytes.

#### Syntax

```
(alignof type)
```

- `type`: The type to get the alignment of.

#### Example

```
(var alignment-of-int (alignof :int)) ; returns the alignment of an integer
```

## Type System

The IR supports a simple type system with type checking and inference. To distinguish between built-in and user-defined types, built-in types are represented as Common Lisp keywords (e.g., `:int`), while user-defined types are represented as regular Common Lisp symbols.

### Primitive Types

- `:int` - Integer values
- `:float` - Floating-point values
- `:bool` - Boolean values (true/false)
- `:string` - String values
- `:void` - No value (for functions that don't return)

### Type Annotations

Types can be explicitly specified when needed:

```
(var x :int 5)
(var y :float 3.14)
(var flag :bool true)
```

## Comments

Comments start with `;` and extend to the end of the line.

### Example

```
(var x int 5)  ; This is a comment
; This is also a comment
```

## Context Options

The IR provides a way to set GCC JIT compilation options using the `options` form. This allows fine-tuning of the compilation process.

### Syntax

```
(options (option-name value)
         (option-name value)
         ...)
```

- `option-name`: A keyword representing the option to set. Can be a string option (`:str-option-*`), integer option (`:int-option-*`), or boolean option (`:bool-option-*`).
- `value`: The value to set for the option. The type depends on the option kind.

### Example

```
(options (:int-option-optimization-level 1)
         (:bool-option-debuginfo true)
         (:bool-option-dump-generated-code true))
```

This example sets the optimization level to 1, enables debug information, and enables dumping of generated code.

## Complete Example

Here's a complete example showing a function that calculates the factorial of a number:

```
(defun factorial (n) -> :int
  (block entry
    (var result :int 1)
    (var i :int 1)
    (jump check))

  (block loop
    (set result (* result i))
    (set i (+ i 1))
    (jump check))

  (block check
    (if (<= i n)
      (jump loop)
      (jump end)))

  (block end
    (return result)))
```

This example demonstrates:
- Function definition with parameters and return type
- Variable declarations with explicit types
- Variable assignments
- Arithmetic expressions
- Conditional branching with if-else
- Unconditional jumps with jump
- Return statement

## Additional Examples

### Simple Function with Multiple Parameters

```
(defun rectangle-area (width height) -> :int
  (block entry
    (return (* width height))))
```

### Conditional Logic with Multiple Branches

```
(defun max (a b) -> :int
  (block entry
    (if (> a b)
      (jump a-is-larger)
      (jump b-is-larger-or-equal)))

  (block a-is-larger
    (return a))

  (block b-is-larger-or-equal
    (return b)))
```

### Boolean Logic Function

```
(defun is-even (n) -> :bool
  (block entry
    (var remainder :int (% n 2))
    (if (= remainder 0)
      (jump even)
      (jump odd)))

  (block even
    (return true))

  (block odd
    (return false)))
```

### Complex Control Flow with Nested Conditions

```
(defun fizzbuzz-value (n) -> :string
  (block entry
    (var divisible-by-3 :bool (= (% n 3) 0))
    (var divisible-by-5 :bool (= (% n 5) 0))
    (if (and divisible-by-3 divisible-by-5)
      (jump fizzbuzz-case)
      (if divisible-by-3
        (jump fizz-case)
        (if divisible-by-5
          (jump buzz-case)
          (jump number-case)))))

  (block fizzbuzz-case
    (return "FizzBuzz"))

  (block fizz-case
    (return "Fizz"))

  (block buzz-case
    (return "Buzz"))

  (block number-case
    (return (int-to-string n))))
```

### Recursive Function Implementation

```
(defun fibonacci (n) -> :int
  (block entry
    (if (<= n 1)
      (jump base-case)
      (jump recursive-case)))

  (block base-case
    (return n))

  (block recursive-case
    (var n-minus-1 :int (- n 1))
    (var n-minus-2 :int (- n 2))
    (var fib-n-minus-1 :int (fibonacci n-minus-1))
    (var fib-n-minus-2 :int (fibonacci n-minus-2))
    (return (+ fib-n-minus-1 fib-n-minus-2))))
```

## Advanced Examples

### Function with Void Return Type and Side Effects

```
(defun print-countdown (n) -> :void
  (block entry
    (var i :int n)
    (jump loop))

  (block loop
    (print i)
    (set i (- i 1))
    (if (>= i 0)
      (jump loop)
      (jump end)))

  (block end
    (print "Done!")
    (return)))
```

### Working with Floating-Point Numbers

```
(defun calculate-circle-area (radius) -> :float
  (block entry
    (var pi :float 3.14159)
    (var radius-squared :float (* radius radius))
    (return (* pi radius-squared))))
```

### Complex Expression with Multiple Operations

```
(defun quadratic-formula ((a :float) (b :float) (c :float) (x :float)) -> :float
  (block entry
    (var x-squared :float (* x x))
    (var a-times-x-squared :float (* a x-squared))
    (var b-times-x :float (* b x))
    (var result :float (+ (+ a-times-x-squared b-times-x) c))
    (return result)))
```

### Function Calling Another Function

```
(defun sum-of-squares (a b) -> :int
  (block entry
    (var a-squared :int (square a))
    (var b-squared :int (square b))
    (return (+ a-squared b-squared))))
```

These advanced examples demonstrate how the IR language can handle more complex scenarios including:
- Void return types and side effects (like printing)
- Floating-point arithmetic
- Complex expressions with multiple operations
- Function composition through function calls


## Relationship to GCC JIT FFI

The high-level IR is designed to map directly to the low-level GCC JIT FFI constructs. Here's how some IR constructs correspond to FFI calls:

| IR Construct | GCC JIT FFI Equivalent |
|-------------|------------------------|
| `(defun name (params) -> return-type ...)` | `context-new-function` |
| `(block name ...)` | `function-new-block` |
| `(var name :type value)` | `function-new-local` + `block-add-assignment` |
| `(set var value)` | `block-add-assignment` |
| `(+ a b)` | `context-new-binary-op` with `:binary-op-plus` |
| `(if condition (jump true-block) (jump false-block))` | `block-end-with-conditional` |
| `(jump target-block)` | `block-end-with-jump` |
| `(switch expr ...)` | `block-end-with-switch` |
| `(return value)` | `block-end-with-return` |
| `(function-name arg1 arg2 ... argN)` | `context-new-call` |
| `(cast expr type)` | `context-new-cast` |
| `(ref lvalue)` | `lvalue-get-address` |
| `(deref rvalue)` | `rvalue-dereference` |
| `(sizeof type)` | `context-new-sizeof` |
| `(alignof type)` | `context-new-alignof` |
| `(array-type E N)` | `context-new-array-type` |
| `(at array idx)` | `context-new-array-access` |
| `(global ...)` | `context-new-global` |
| `(struct name ...)` | `context-new-struct-type` |
| `(field struct field-name)` | `rvalue-access-field` |
| `(options ...)` | `context-set-str-option`, `context-set-int-option`, or `context-set-bool-option` |

This direct mapping makes the compilation from IR to machine code straightforward while providing a much more user-friendly syntax than working directly with the FFI.
 straightforward while providing a much more user-friendly syntax than working directly with the FFI.
