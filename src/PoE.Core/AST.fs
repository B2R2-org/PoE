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

open System.Collections

/// Bitvector.
type BitVector = BitArray

/// Basic expression.
type Expr =
  /// Variable.
  | Var of string * Annotation
  /// Bit-vector.
  | BitVector of BitVector * Annotation
  /// 64-bit integer value.
  | Int of int64 * ExprType * Annotation
  /// Unit value.
  | Unit
  /// Boolean expression.
  | BoolExpr of BoolExpr
  /// Arithmetic expression.
  | ArithExpr of ArithExpr
  /// Logic expression.
  | LogicExpr of LogicExpr
  /// Type casting.
  | TypeCast of ExprType * Expr * Annotation
  /// Extract a sub-bitvector.
  | Extract of Expr * soff: Expr * eoff: Expr * Annotation
  /// Concatenation of two bitvectors.
  | Concat of Expr * Expr * Annotation
  /// Multiplied bitvector.
  | BVMult of v: Expr * multiplier: Expr * Annotation
  /// If-then-else.
  | Ite of Expr * Expr * Expr * Annotation
  /// Call a function.
  | Call of string * Expr list * Annotation
  /// Assembly expression.
  | AsmExpr of arch: string * asm: string * replacements: Expr list * Annotation

and BoolExpr =
  /// Equal.
  | Eq of Expr * Expr * Annotation
  /// Not equal.
  | Neq of Expr * Expr * Annotation
  /// Greater than.
  | Gt of Expr * Expr * Annotation
  /// Greater than and equal to.
  | Ge of Expr * Expr * Annotation
  /// Less than.
  | Lt of Expr * Expr * Annotation
  /// Less than and equal to.
  | Le of Expr * Expr * Annotation

and ArithExpr =
  /// Negative value.
  | Neg of Expr * Annotation
  /// Addition.
  | Add of Expr * Expr * Annotation
  /// Subtraction.
  | Sub of Expr * Expr * Annotation
  /// Multiplication.
  | Mul of Expr * Expr * Annotation
  /// Division.
  | Div of Expr * Expr * Annotation
  /// Remainder.
  | Mod of Expr * Expr * Annotation

and LogicExpr =
  /// Shift left.
  | Shl of Expr * Expr * Annotation
  /// Shift right.
  | Shr of Expr * Expr * Annotation
  /// Logical AND.
  | And of Expr * Expr * Annotation
  /// Logical OR.
  | Or of Expr * Expr * Annotation
  /// Logical XOR.
  | Xor of Expr * Expr * Annotation
  /// Logical negation.
  | Not of Expr * Annotation

type DebugFormat =
  | OutHex
  | OutAscii

type Stmt =
  /// Variable definition (a value is associated with the var).
  | VarStmt of ExprType * (string * Expr) list * Annotation
  /// Make a call (to either an act or a fun).
  | CallStmt of string * Expr list * Annotation
  /// Return a value from a function or an act.
  | Return of Expr * Annotation
  /// Variable assignment.
  | AssignStmt of string * Expr * Annotation
  /// For-loop statement.
  | ForStmt of string * init: Expr * fini: Expr * body: Stmt list * Annotation
  /// While-loop statement.
  | WhileStmt of cond: Expr * body: Stmt list * Annotation
  /// Conditional (if-then-else) statement.
  | CondStmt of cond: Expr * tbody: Stmt list * fbody: Stmt list * Annotation
  /// Break the loop.
  | BreakStmt of Annotation
  /// No-op (nop) statement.
  | NopStmt of Annotation
  /// SMT-Solve statement.
  | SolveStmt of Expr * Annotation
  /// Print out a string for debugging purpose.
  | DebugStmt of Expr * DebugFormat * Annotation

type Declaration =
  /// Global var(s) declaration.
  | GlobalVarDecl of ExprType * (string * Expr) list * Annotation
  /// Function declaration.
  | FunDecl of string * (Expr * ExprType) list * Stmt list * Annotation
  /// Act declaration. Unlike a function, an act involves creating a stream.
  | ActDecl of string * (Expr * ExprType) list * Stmt list * Annotation
  /// Submit declaration.
  | SubmitDecl of Stmt list * Annotation

type Macro =
  /// Replace the value of the nth write with "src". This will only overwrite a
  /// part of the write value specified by two bit offsets (sOff and eOff).
  | Overwrite of nth: int * src: BitVector * sOff: int * eOff: int

/// Proof-of-Exploit.
type PoE = {
  /// Target binary's ID.
  ChallengeBinaryID: string
  /// This is an extra information indicating what kind of exploit type this PoE
  /// handles.
  ExploitType: ExploitType
  /// PoE is a sequence of declarations.
  Decls: Declaration list
  /// Macros.
  Macros: Macro list
}
