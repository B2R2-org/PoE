Level 01
===

In this tutorial, you will learn how to use variables and manipulate them, and
how to construct complex logic in PoE.

## Variables and Assignments

You can declare a variable to hold a value of a specific type. A variable can
hold different values of the same type.

#### Variable declaration

You can declare a variable with the following syntax: `(type) (name)`. For
example, `u8 x` declares a variable `x` of type `u8`. In this case, `x` can only
hold unsigned 8-bit integers. You can also declare multiple variables of the
same type separated by commas. For example, `i64 x, y, z` declares the three
variables `x`, `y`, and `z` of type `i64`.

#### Variable initialization

You can initialize variables while declaring them. Let us suppose we want to
declare two `i64` variables `x` and `y`, while initializing their values as 4
and 2, respectively. We can use the following syntax to achieve it: `i64 x = 4,
y = 2`.

#### Variable assignment

You can assign a value to a variable as long as the value has the same type with
the variable. The assignment operator (`:=`) can be used to achieve this. For
example, `x := 42` will assign the number 42 to the variable `x`.  Note that
assignment only works when both the source and the destination types are the
same.

```
u32 a = 1
a := 42:i32 // this will fail
a := 42     // but, this works
a := -1     // this also works
```

The assignment statement in the above example will fail because `a` has the type
`u32` whereas the source operand has the value of type `i32`. However, the
second assignment statement will work because the number `42` has no explicit
type attached to it, so the PoE interpreter will implicitly cast its type to
`u32`. The third assignment also works due to the implicit type casting, and the
resulting value of `a` after the third assignment statement is `0xffffffff` (of
type `u32`).

## Type Casting

Sometimes, it is desirable to be able to explicitly type-cast a value. For
example, you may want to add two numbers of two different types. For explicit
type conversion, we use the following syntax: `(type) (expr)`.

```
u32 a = 42
i32 b = a + 1     // type error.
i32 b = i32 a + 1 // type-check because `a` has been explicitly type-casted.
```

In the above example, the second statement does not type-check because `a` and
`b` have different types. However, the third statement type-checks because it
explicitly type-casts the variable `a` (from `u32` to `i32`).

## Bitvector Substitution

PoE allows bit-wise substitution of a bitvector. Substitution uses the
assignment operator `:=`, but the left-hand side operand should be in the form
of extraction expression.

```
bv v = 0x11223344:i32 // must type-cast to a bitvector to perform substitution
v[0] := 1bs  // substitute the LSB to 1
v[0:0] := 11bs // this will cause run-time error
v[0:1] := 11bs // works: substitute the first two bits
```

In the above example, the first substitution statement will replace the LSB of
0x11223344 to 1, thus `v` will contain 0x11223345 after it. The second
substitution statement, however, raises a runtime error because the destination
operand specifies a single bit ([0:0]) whereas the source operand has a 2-bit
value. The third substitution statement will substitute the first two bits of
`v`, making it 0x11223347.

## If-Then-Else

The if-then-else expression conditionally evaluates a value. Both branches
should have the same type. For example, `if x = y then 1 else 2` is a valid
expression, but `if x = y then 1:i32 else 2:u32` is not.

As in most other language, PoE also has if-then-else statements, which do not
produce any value. For example, you can conditionally have assignment
statements:

```
if x > 42 then x := 42
else x := x + 1
```

In PoE, however, both `if` and `else` clause should coexist. It is not possible
to have only an `if` clause as in other languages. So if you really want to do
nothing in an else branch, for instance, then you can simply put a void value in
the else clause. For example,

```
if x > 42 then
  x := 42
  y := 42
else () // in else branch, do nothing.
```

## Loops

PoE supports both for-loops and while-loops.

#### For-Loops

For-loops in PoE have a loop variable, which controls the number of iterations.
The body of a for-loop should follow the head with a proper indentation.

```
bv a = 0x11223344:i32
for i = 0 to 7
    a[i] := 1bs // indentation is necessary
```

In this example, the for-loop sets the lowest eight bits (with eight
iterations). The resulting value of `a` after the loop is `0x112233FF`. The loop
variable `i` will only be accessible within the loop body.

#### While-Loops

While-loops in PoE is similar to those of other languages. The body of a
while-loop should follow the head with a proper indentation as in for-loops.

```
bv a = 0x11223344:i32
i64 i = 7
while i >= 0
    a[i] := 1bs
    i := i - 1
```

The above example sets the lowest eight bits of `a` to `0xff` using a
while-loop.

#### Break

Both for-loops and while-loops can have a `break` statement, which allows the
program to break out of the loop. For example, the following is valid PoE
snippet.

```
while i > 0
   i := i - 1
   if x > 42 then
     break
   else ()
```

## Functions and Acts

#### Functions

You can define a function using the `fun` keyword. Functions can take zero or
more parameters. A function body should always have a proper indentation, and
ends with one or more `return` statement. PoE does not allow tab characters for
indentation.

```
fun inc(n: u64):
    return n + 1
```

The above shows a function that takes in a `u64` number as input, and returns an
incremented `u64` number as output.

#### Acts

An act is a special function in PoE, which is defined with the `act` keyword. It
behaves the same as a function with one exception: it initiates a session
whenever it is called.

```
act exploit():
    return 42
```

Here, a session means two different things depending on the commands used to run
the interpreter.

1. When the interpreter runs against a local process with `stdin` and `stdout`,
   a session here means launching a new process. That is, whenever the
   interpreter executes an act, it will create a new process.

2. When the interpreter runs against a remote process with a socket, a session
   means connecting to the socket. That is, whenever the interpreter executes an
   act, it will make a new connection to the specified IP and port.

Typically, having a single act in a PoE is enough unless your exploit requires
multiple connections. However, it is always a good practice to have multiple
functions to refactor your code and make it more readable.
