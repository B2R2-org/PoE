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

module PoE.ASTUtils

open System.Text.RegularExpressions
open PoE.BitVectorUtils

/// Replace a part of the content of the given write statement. The part starts
/// at the sOff and ends at the eOff.
let replaceWrite stmt sOff eOff src =
  let src = BitVector (bytesToBV src, None)
  let sOff = Int (sOff, TypeInt64, None)
  let eOff = Int (eOff, TypeInt64, None)
  match stmt with
  | CallStmt ("write", [ e ], annot) ->
    CallStmt ("write", [ Call ("replace", [ e; src; sOff; eOff ], None) ], None)
  | _ -> raise (RuntimeException "Can only replace write statement.")

let getAnnotationFromStmt = function
  | VarStmt (_, _, annot)
  | CallStmt (_, _, annot)
  | Return (_, annot)
  | AssignStmt (_, _, annot)
  | ForStmt (_, _, _, _, annot)
  | WhileStmt (_, _, annot)
  | CondStmt (_, _, _, annot)
  | BreakStmt (annot)
  | NopStmt (annot)
  | SolveStmt (_, annot)
  | DebugStmt (_, _, annot) -> annot

let getAnnotationFromDecl = function
  | GlobalVarDecl (_, _, annot)
  | FunDecl (_, _, _, annot)
  | ActDecl (_, _, _, annot)
  | SubmitDecl (_, annot) -> annot

let [<Literal>] asmVarPlaceholder = '%'

let assertNonEmptyBlock stmts annot =
  if List.isEmpty stmts then
    let annot = Option.map (fun n -> n + 1) annot
    raise (ParsingException ("Invalid block stmt.", annot))
  else ()

let countAsmPlaceholders asm =
  Regex.Matches(asm, string asmVarPlaceholder).Count
