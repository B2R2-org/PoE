Level 00
===

PoE (Proof-of-Exploit) is a language that is specifically designed for writing
an exploit. In this tutorial, you will learn the basic syntax of PoE.

## Types and Values

PoE is a well-typed language. Therefore, any value must have a specific type
attached to it. There are roughly two main types in PoE: numbers or
bitvectors. Numbers can be either unsigned or signed integers, and can represent
maximum 64-bit values. On the other hand, bitvector types can be in any
arbitrary size. The below table summarizes all possible types in PoE.

| Types | Meaning                      |
|-------|------------------------------|
| `u8`  | unsigned 8-bit integer       |
| `i8`  | signed 8-bit integer         |
| `u16` | unsigned 16-bit integer      |
| `i16` | signed 16-bit integer        |
| `u32` | unsigned 32-bit integer      |
| `i32` | signed 32-bit integer        |
| `u64` | unsigned 64-bit integer      |
| `i64` | signed 64-bit integer        |
| `bv`  | bitvector of a specific size |

Numbers are represented in two different formats: either decimal or
hexadecimal. Hexadecimal representation always starts with `0x`. Numbers can be
type-annotated with `:` to be specific, but it can be omitted. For example, both
`42` and `42:i64` are valid. The type of the first expression (without type
annotation) will be inferred by the interpreter based on its usage.

Bitvectors are represented in various ways. The most basic way is to represent a
binary number (either 0 or 1) with a suffix `bs`. For example, `01000010bs` is
an 8-bit bitvector, which is equivalent to `42` in hexadecimal. You can use a
hexadecimal string (with a suffix `hs`). For example, `424242hs` is a 3-byte
bitvector. You can put as many as hex-characters before `hs` to construct an
arbitrary-size bitvector. You can also use string literal with double quotes to
represent a bitvector. For example, "hello" is a 5-byte bitvector. Special
characters such as a newline character (`\n`) can be placed within a string
representation. Note our string representation does not put a null byte at the
end.

| Bitvector Representation | Size    | Meaning                              |
|--------------------------|---------|--------------------------------------|
| `01000010bs`             | 1 byte  | raw binary sequence                  |
| `"42"`                   | 2 bytes | ASCII string                         |
| `42hs`                   | 1 byte  | raw byte sequence (in hex)           |
| `true`                   | 1 bit   | one-bit representing true (= `1bs`)  |
| `false`                  | 1 bit   | one-bit representing false (= `0bs`) |
| `()`                     | 0 bit   | void, meaning nothing                |

Note, all the above literals have a bitvector type, although the syntax differs.

## Operators

PoE supports most regular arithmetic/bitwise operators, such as `+`, `-`, `*`,
`/`, `%`, and etc. Especially, all arithmetic operators assume that operands
have the *same* number type. Otherwise, a type error will be raised.

For example, `1:i32 + 1:u32` will be accepted by the type-checker. Also,
`1:i32 + 1` will be accepted, since `1` will be implicitly converted to `1:i32`
by the interpreter. However, `42hs + 42hs` will be rejected because the addition
operator will not work with bitvector types.

The below table summarizes all the operators defined in PoE:

| Operator | Meaning               | Operand Kind |
|----------|-----------------------|--------------|
| `+`      | Plus                  | Number       |
| `-`      | Minus                 | Number       |
| `/`      | Divide                | Number       |
| `*`      | Multiply              | Number       |
| `%`      | Modulo                | Number       |
| `<<`     | Shift-left            | Number       |
| `>>`     | Shift-right           | Number       |
| `&`      | Bitwise And           | Number       |
| `\|`     | Bitwise Or            | Number       |
| `^`      | Bitwise Xor           | Number       |
| `~`      | Bitwise Not           | Number       |
| `<`      | Less-than             | Number       |
| `>`      | Greater-than          | Number       |
| `<=`     | Less-than or equal    | Number       |
| `>=`     | Greater-than or equal | Number       |
| `and`    | Logical And           | Boolean      |
| `or`     | Logical Or            | Boolean      |
| `xor`    | Logical Xor           | Boolean      |
| `not`    | Logical Not           | Boolean      |
| `=`      | Equal                 | Any          |
| `<>`     | Not equal             | Any          |

In the above table, `Any` means that the operator works with any type. `Boolean`
means that the operator works with boolean values only (`true` or `false`),
which are actually one-bit bitvectors.

## Evaluating Values

Let's now write some values in PoE for your exercise. We will use
`PoE.Replayer`, so please [install](../../README.md#installation) it first.

With `PoE.Replayer`, you can evaluate any PoE expression in any terminal or any
OS. Simply type `poe eval "<expr>"`, where `<expr>` is a PoE expression. Below
are some examples. Note that an evaluated value will be output in two different
formats. When the resulting type is a number type, it shows both decimal and
hexadecimal representations. When the resulting type is a bitvector type, it
shows both hexadecimal and ASCII representation.

```
> poe eval "1 + 2 * 3 + 4"
dec   = {11}
hex   = {b}

> poe eval "404142434445464748494a4b4c4d4e4fhs"
hex   = {404142434445464748494a4b4c4d4e4f}
ascii = {@ABCDEFGHIJKLMNO}
```

Every time you launch a command `poe eval`, the resulting value will be stored
in the `payload.dat` file in your current working directory. This way, you can
easily use the generated value as input to the program you are exploiting.

It is also important to note `poe eval` should always be followed by a
double-quoted argument. This is to make sure that your shell does not falsely
interpret the given argument. For example, suppose you want to evaluate a
comparison expression with `poe eval 1 > 2`. If you don't use double quotes,
this command is interpreted as `poe eval 1`, and the output of the command will
be stored in a file named `2` because `>` is a special operator that redirects
the output to a file.

## Manipulating Bitvectors

#### String Multiplication

Bitvectors declared with double quotes can be multiplied by a special operator
`x`. For example, you can create 100 'A's by `"A"x100`. This can be extremely
useful when writing a buffer-overflow exploit. The multiplier can be a compound
expression, too. For example, `"A"x(50 + 50)` will also produce 100 'A's.

#### Bitvector Concatenation

Bitvectors can be concatenated to create a new one. PoE provides a convenient
operator `.` (dot) to perform concatenation. The concatenation operator works
also for numbers, as it will implicitly type cast numbers into
bitvectors. Therefore, the result of concatenation will always be a bitvector
even though the operands are given as a number. For example, you can concatenate
two `u8` numbers to get a 16-bit bitvector:
```
> poe eval "0x42:u8 . 0x42:u8"
hex   = {4242}
ascii = {BB}
```

It is a good practice to use both multiplication and concatenation in order to
construct a complex payload in a concise manner.

#### Bitvector Extraction

You can also extract a part of a value of an expression using the following
syntax: `expr[soff:eoff]`, where `soff` and `eoff` is a start and end bit
offset, respectively. For example, `"hello"[0:7]` will return a one-byte (or
eight-bit) bitvector containing `h`: it extracts a sub-bitvector starting at
offset 0 and ends at offset 7.

You can also extract a bitvector from a number as PoE will implicitly type-cast
a number into a bitvector before extraction. For example, you can take the lower
16 bits of a 32-bit number: `0x11223344:i32[0:15]` will return `4433hs` as the
number is stored in little endian.

It is often desired to extract higher bits without having to know the exact
length of the bitvector. For this purpose, you can use negative bit offsets,
which means the offset from the MSB. For example, `0x11223344[-1:-1]` will
return the MSB, and `0x11223344[-8:-1]` will return the most significant byte
(0x11). Here, `-1` means the first bit when counting from MSB, that is the MSB's
offset. Similarly, `-8` means the eighth bit when counting from MSB. Thus, it
takes the highest 8 bits from the number 0x11223344, which corresponds to 0x11.

It is also possible to use both positive and negative offsets. For example,
`0x11223344[0:-1]` is a valid extraction expression which will basically extract
the whole number (from the LSB to the MSB). Similarly, `0x11223344[0:-9]` will
extract the first 3 bytes from the number, returning `443322hs`.

## Why PoE?

Although you have seen only the tip of an iceberg, it is good time to discuss
why PoE allows us to write a concise and intuitive exploit.

Say you are writing a ROP payload in Python. You would construct it by
concatenating a series of gadget addresses and numbers (which will be loaded in
a register with a pop-ret gadget). Theoretically, the payload looks like:

```
| gadget_1 | value_1 | value_2 | gadget_2 | gadget_3 | ...
```

To construct such a payload with Python, you have to use `struct.pack` or
similar functions to make a proper byte representation. However, the problem is
that if the `value_1` or `value_2` are a negative number, you have to build the
corresponding byte sequence by hand as `struct.pack` will not take a negative
integer as input. For example, `struct.pack(">Q", -10)` will not work.

With PoE, however, you can simply construct the above payload without having to
worry about packing or unpacking data. Concatenating an address and a negative
number is simply done with a concatenation operator. For example, suppose the
`value_1` in the above payload is `-10` (64-bit signed integer), then the
payload can be written in PoE as follows:

```
gadget_1 . -10:i64 . value_2 . gadget_2 . gadget_3
```

As you can see, our payload is intuitive and concise. In fact, the type
annotation `:i64` can also be omitted, as the default integer length is 64-bit.
