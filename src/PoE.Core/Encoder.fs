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

module PoE.Encoder

open System
open System.Text

let private indentShiftAmount = 2

let append (builder: StringBuilder) (str: string) =
  builder.Append (str) |> ignore

let ppType builder typ =
  match typ with
  | TypeAnyBV | TypeBV _ -> append builder "bv"
  | TypeInt8 -> append builder "i8"
  | TypeInt16 -> append builder "i16"
  | TypeInt32 -> append builder "i32"
  | TypeInt64 -> append builder "i64"
  | TypeUInt8 -> append builder "u8"
  | TypeUInt16 -> append builder "u16"
  | TypeUInt32 -> append builder "u32"
  | TypeUInt64 -> append builder "u64"
  | TypeUnit -> raise (EncodingException "Invalid use of unit type.")
  | TypeFunction _ -> raise (EncodingException "Invalid use of function type.")
  | TypeAny | TypeAnyInt ->
    raise (EncodingException "Invalid type.")

let ppBitVectorAsHexString builder (bv: BitVector) =
  let mutable num = 0
  let mutable count = 0
  for i = 0 to bv.Length - 1 do
    if bv.[i] then num <- num + (1 <<< count) else ()
    count <- count + 1
    if count % 8 <> 0 then ()
    else
      append builder (num.ToString ("X2"))
      count <- 0
      num <- 0
  done
  append builder "hs"

let ppBool builder (bv: BitVector) =
  if bv.[0] then append builder ("true")
  else append builder "false"

let ppBitVectorAsBinaryString _builder _bv =
  failwith "implement"

let ppBitVector builder (bv: BitVector) =
  if bv.Length = 0 then append builder "\"\""
  elif bv.Length % 8 = 0 then ppBitVectorAsHexString builder bv
  elif bv.Length = 1 then ppBool builder bv
  else ppBitVectorAsBinaryString builder bv

let rec ppExpr builder = function
  | Var (name, _) -> append builder name
  | BitVector (bv, _) -> ppBitVector builder bv
  | Int (n, typ, _) ->
    if ExprType.isConcreteIntType typ then
      append builder (n.ToString () + ":" + ExprType.toString typ)
    else append builder (n.ToString ())
  | Unit -> append builder "()"
  | BoolExpr e -> ppBoolExpr builder e
  | ArithExpr e -> ppArithExpr builder e
  | LogicExpr e -> ppLogicExpr builder e
  | TypeCast (t, e, _) -> ppAppWrap builder (ExprType.toString t) [] e
  | Extract (e, soff, eoff, _) -> ppExtract builder e soff eoff
  | Concat (lhs, rhs, _) -> ppConcat builder lhs rhs
  | BVMult (v, multiplier, _) -> ppBVMult builder v multiplier
  | Ite (cond, texp, fexp, _) -> ppITE builder cond texp fexp
  | Call (name, args, _) -> ppCall builder name args
  | AsmExpr (arch, asm, replacements, _) -> ppAsm builder arch asm replacements

and ppBoolExpr builder = function
  | Eq (lhs, rhs, _) -> ppBinOp builder lhs rhs "="
  | Neq (lhs, rhs, _) -> ppBinOp builder lhs rhs "<>"
  | Gt (lhs, rhs, _) -> ppBinOp builder lhs rhs ">"
  | Ge (lhs, rhs, _) -> ppBinOp builder lhs rhs ">="
  | Lt (lhs, rhs, _) -> ppBinOp builder lhs rhs "<"
  | Le (lhs, rhs, _) -> ppBinOp builder lhs rhs "<="

and ppArithExpr builder = function
  | Neg (e, _) -> ppUnOp builder e "-"
  | Add (lhs, rhs, _) -> ppBinOp builder lhs rhs "+"
  | Sub (lhs, rhs, _) -> ppBinOp builder lhs rhs "-"
  | Mul (lhs, rhs, _) -> ppBinOp builder lhs rhs "*"
  | Div (lhs, rhs, _) -> ppBinOp builder lhs rhs "/"
  | Mod (lhs, rhs, _) -> ppBinOp builder lhs rhs "%"
  | BAnd (lhs, rhs, _) -> ppBinOp builder lhs rhs "&"
  | BOr (lhs, rhs, _) -> ppBinOp builder lhs rhs "|"
  | BXor (lhs, rhs, _) -> ppBinOp builder lhs rhs "^"
  | BNot (e, _) -> ppUnOp builder e "~"

and ppLogicExpr builder = function
  | Shl (lhs, rhs, _) -> ppBinOp builder lhs rhs "<<"
  | Shr (lhs, rhs, _) -> ppBinOp builder lhs rhs ">>"
  | And (lhs, rhs, _) -> ppBinOp builder lhs rhs "and"
  | Or (lhs, rhs, _) -> ppBinOp builder lhs rhs "or"
  | Xor (lhs, rhs, _) -> ppBinOp builder lhs rhs "xor"
  | Not (e, _) -> ppUnOp builder e "not"

and ppAppWrap builder keyword args e =
  append builder "("
  append builder keyword
  append builder " "
  List.iter (fun arg -> append builder arg; append builder " ") args
  ppExpr builder e
  append builder ")"

and ppExtract builder e soff eoff =
  ppExpr builder e
  append builder "["
  ppExpr builder soff
  append builder ":"
  ppExpr builder eoff
  append builder "]"

and ppConcat builder lhs rhs =
  append builder "("
  ppExpr builder lhs
  append builder " . "
  ppExpr builder rhs
  append builder ")"

and ppBVMult builder v multiplier =
  append builder "\""
  match v with
  | BitVector (bv, _) ->
    BitVectorUtils.bvToBytes bv
    |> StringUtils.bytesToStr
    |> append builder
  | _ -> raise (EncodingException "Invalid BVMult expr.")
  append builder "\"x("
  ppExpr builder multiplier
  append builder ")"

and ppITE builder cond texp fexp =
  append builder "(if "
  ppExpr builder cond
  append builder " then "
  ppExpr builder texp
  append builder " else "
  ppExpr builder fexp
  append builder ")"

and ppArgs builder args =
  args
  |> List.iteri (fun idx arg ->
    if idx > 0 then append builder ", " else ()
    ppExpr builder arg)
  append builder ")"

and ppCall builder name args =
  append builder name
  append builder "("
  ppArgs builder args

and ppAsm builder arch asm replacements =
  append builder arch
  append builder "{{"
  append builder asm
  append builder "}}"
  if List.isEmpty replacements then ()
  else
    append builder ":("
    ppArgs builder replacements

and ppUnOp builder e operator =
  append builder (operator + " (")
  ppExpr builder e
  append builder ")"

and ppBinOp builder lhs rhs operator =
  append builder "("
  ppExpr builder lhs
  append builder (" " + operator + " ")
  ppExpr builder rhs
  append builder ")"

let ppIndent builder indent =
  for i = 1 to indent do
    append builder " "
  done

let ppElems builder args =
  let p idx (arg, typ) =
    if idx = 0 then () else append builder ", "
    ppExpr builder arg
    if ExprType.isAnyType typ then ()
    else append builder ":"; ppType builder typ
  List.iteri p args

let ppVars builder vars =
  let p idx (name, e) =
    if idx = 0 then () else append builder ", "
    append builder name
    match e with
    | Unit -> ()
    | _ ->
      append builder " = "
      ppExpr builder e
  List.iteri p vars

let rec ppStmt builder indent = function
  | VarStmt (typ, vars, _) ->
    ppIndent builder indent
    ppType builder typ
    append builder " "
    ppVars builder vars
    append builder Environment.NewLine
  | CallStmt (fnName, args, _) ->
    ppIndent builder indent
    ppCall builder fnName args
    append builder Environment.NewLine
  | Return (e, _) ->
    ppIndent builder indent
    append builder "return "
    ppExpr builder e
    append builder Environment.NewLine
  | AssignStmt (v, e, _) ->
    ppIndent builder indent
    append builder v
    append builder " := "
    ppExpr builder e
    append builder Environment.NewLine
  | ForStmt (v, init, fini, body, _) ->
    ppIndent builder indent
    append builder "for "
    append builder v
    append builder " = "
    ppExpr builder init
    append builder " to "
    ppExpr builder fini
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) body
    append builder Environment.NewLine
  | WhileStmt (cond, body, _) ->
    ppIndent builder indent
    append builder "while "
    ppExpr builder cond
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) body
    append builder Environment.NewLine
  | CondStmt (cond, tbody, fbody, _) ->
    ppIndent builder indent
    append builder "if "
    ppExpr builder cond
    append builder " then"
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) tbody
    if List.isEmpty fbody then ()
    else
      append builder Environment.NewLine
      ppIndent builder indent
      append builder "else"
      append builder Environment.NewLine
      ppStmts builder (indent + indentShiftAmount) fbody
    append builder Environment.NewLine
  | BreakStmt (_) ->
    ppIndent builder indent
    append builder "break"
    append builder Environment.NewLine
  | NopStmt (_) ->
    ppIndent builder indent
    append builder "()"
    append builder Environment.NewLine
  | SolveStmt (e, _) ->
    ppIndent builder indent
    append builder "solve "
    ppExpr builder e
    append builder Environment.NewLine
  | DebugStmt (_, _, _) -> ()

and ppStmts builder indent = function
  | stmt :: rest ->
    ppStmt builder indent stmt
    ppStmts builder indent rest
  | [] -> ()

let ppDecl builder indent = function
  | GlobalVarDecl (typ, vars, _) ->
    ppIndent builder indent
    ppType builder typ
    append builder " "
    ppVars builder vars
    append builder Environment.NewLine
    indent
  | FunDecl (name, args, stmts, _) ->
    ppIndent builder indent
    append builder "fun "
    append builder name
    append builder " ("
    ppElems builder args
    append builder "):"
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) stmts
    append builder Environment.NewLine
    indent
  | ActDecl (name, args, stmts, _) ->
    ppIndent builder indent
    append builder "act "
    append builder name
    append builder " ("
    ppElems builder args
    append builder "):"
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) stmts
    append builder Environment.NewLine
    indent
  | SubmitDecl (stmts, _) ->
    ppIndent builder indent
    append builder "submit:"
    append builder Environment.NewLine
    ppStmts builder (indent + indentShiftAmount) stmts
    append builder Environment.NewLine
    indent

let appendExploitType builder = function
  | Unexploitable -> ()
  | t ->
    append builder <| ExploitType.toString t
    append builder Environment.NewLine

let rec appendMacros builder = function
  | Overwrite (nth, bv, sOff, eOff) :: rest ->
    append builder "#overwrite "
    append builder (nth.ToString ())
    append builder " "
    ppBitVector builder bv
    append builder " "
    append builder (sOff.ToString ())
    append builder " "
    append builder (eOff.ToString ())
    append builder Environment.NewLine
    appendMacros builder rest
  | [] -> ()

/// Pretty-print the PoE.
let toString { ExploitType = etype; Macros = macros; Decls = decls } =
  let builder = StringBuilder ()
  appendExploitType builder etype
  appendMacros builder macros
  List.fold (ppDecl builder) 0 decls |> ignore
  builder.ToString ()
