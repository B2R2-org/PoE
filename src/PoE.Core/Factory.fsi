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

module PoE.Factory

/// Generate a statement that writes the given byte array to the target binary.
val genWrite: byte [] -> Stmt

/// Generate a statement that reads all outputs from the target binary and
/// stores it to a variable. The third argument indicates whether we want to
/// trim-end the input string.
val genReadAll: string -> int -> bool -> Stmt

/// Generate a statement that reads in some values up to a certain length (len),
/// and stores it to a variable.
val genReadVarByLen: varName: string -> len: int -> Stmt

/// Generate a statement that reads in some values until a delimiter string
/// shows up, and stores it to a variable.
val genReadVarByDelim: varName: string -> delim: byte [] -> Stmt

/// Generate a statment that delays for the given milliseconds.
val genDelay: int -> Stmt

/// Generate a simple act statement, which does not take any parameter.
val genSimpleAct: string -> Stmt list -> Declaration

/// Generate a simple submit statement.
val genSimpleSubmit: string -> Declaration

/// Generate a PoE from challenge binary ID, ExploitType, and a list of Stmts.
val genPoE: cbid: string -> ExploitType -> Declaration list -> PoE

/// Generate a PoE that simply submits unit value.
val genTestCasePoE: cbid: string -> Stmt list -> PoE

/// Generate a PoE for shell spwan
val genShellPoE: string -> ExploitType -> string -> int -> Stmt list -> PoE

/// Generate a PoE for shell cat spwan
val genShellCatPoE: string -> ExploitType -> int -> Stmt list -> PoE

/// Generate a PoE for flag leak
val genLeakPoE: string -> ExploitType -> Stmt list -> PoE

/// Generate a simple leak PoE from a given challenge binary ID and a single
/// payload that triggers the leak.
val genSimpleLeakPoE: cbid: string -> ExploitType -> byte [] -> PoE

/// Generate a simple shell spawn PoE from a given challenge binary ID, flag
/// path and a single payload that spawns a shell.
val genSimpleShellPoE:
     cbid: string
  -> ExploitType
  -> flagPath: string -> int -> byte [] -> PoE

/// Rewrite the given PoE by adding an overwrite macro.
val addOverwriteRule: PoE -> OverwriteRule -> PoE
