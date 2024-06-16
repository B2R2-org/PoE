(*

  Copyright (c) SoftSec Lab. @ KAIST, since 2016

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.

 *)

namespace PoE

open System

/// PoE expression type.
type ExprType =
  /// Any type. This cannot be declared by the user.
  | TypeAny
  /// Bitvector of any size. This cannot be declared by the user.
  | TypeAnyBV
  /// Bitvector type of a specific size.
  | TypeBV of size: int
  /// Integer of any size. This cannot be declared by the user.
  | TypeAnyInt
  /// Int8
  | TypeInt8
  /// Int16
  | TypeInt16
  /// Int32
  | TypeInt32
  /// Int64
  | TypeInt64
  /// UInt8
  | TypeUInt8
  /// UInt16
  | TypeUInt16
  /// UInt32
  | TypeUInt32
  /// UInt64
  | TypeUInt64
  /// Unit.
  | TypeUnit
  /// Function type. The return type is determined at runtime.
  | TypeFunction of ret: ExprType * args: ExprType list

module ExprType =
  let toSize = function
    | TypeAny -> 0
    | TypeAnyBV -> 0
    | TypeBV n -> n
    | TypeAnyInt -> 8
    | TypeInt8 -> 1
    | TypeInt16 -> 2
    | TypeInt32 -> 4
    | TypeInt64 -> 8
    | TypeUInt8 -> 1
    | TypeUInt16 -> 2
    | TypeUInt32 -> 4
    | TypeUInt64 -> 8
    | TypeUnit -> 0
    | TypeFunction _ -> 0

  let toBitSize t =
    toSize t * 8

  let toString = function
    | TypeAny -> invalidArg (nameof ExprType) "Invalid type for toString."
    | TypeAnyBV | TypeBV _ -> "bv"
    | TypeAnyInt -> "i64"
    | TypeInt8 -> "i8"
    | TypeInt16 -> "i16"
    | TypeInt32 -> "i32"
    | TypeInt64 -> "i64"
    | TypeUInt8 -> "u8"
    | TypeUInt16 -> "u16"
    | TypeUInt32 -> "u32"
    | TypeUInt64 -> "u64"
    | TypeUnit -> "unit"
    | TypeFunction _ -> "fun"

  let ofInt64 (n: int64) =
    if int64 Byte.MaxValue >= n && int64 Byte.MinValue <= n then TypeInt8
    elif int64 UInt16.MaxValue >= n && int64 UInt16.MinValue <= n then TypeInt16
    elif int64 UInt32.MaxValue >= n && int64 UInt32.MinValue <= n then TypeInt32
    else TypeInt64

  let isAnyType = function
    | TypeAny
    | TypeAnyBV
    | TypeAnyInt -> true
    | _ -> false

  let isConcreteValueType = function
    | TypeBV _
    | TypeInt8
    | TypeInt16
    | TypeInt32
    | TypeInt64
    | TypeUInt8
    | TypeUInt16
    | TypeUInt32
    | TypeUInt64 -> true
    | _ -> false

  let isConcreteIntType = function
    | TypeInt8
    | TypeInt16
    | TypeInt32
    | TypeInt64
    | TypeUInt8
    | TypeUInt16
    | TypeUInt32
    | TypeUInt64 -> true
    | _ -> false

  let isIntType = function
    | TypeAnyInt
    | TypeInt8
    | TypeInt16
    | TypeInt32
    | TypeInt64
    | TypeUInt8
    | TypeUInt16
    | TypeUInt32
    | TypeUInt64 -> true
    | _ -> false

  let isBVType = function
    | TypeAnyBV
    | TypeBV _ -> true
    | _ -> false

  let isSignedIntType = function
    | TypeInt8
    | TypeInt16
    | TypeInt32
    | TypeInt64 -> true
    | _ -> false

  let isUnsignedIntType = function
    | TypeUInt8
    | TypeUInt16
    | TypeUInt32
    | TypeUInt64 -> true
    | _ -> false

  let ofBitSize size =
    if size > 64 then invalidArg (nameof ExprType) "Invalid bit size given."
    elif size > 32 then TypeInt64
    elif size > 16 then TypeInt32
    elif size > 8 then TypeInt16
    else TypeInt8
