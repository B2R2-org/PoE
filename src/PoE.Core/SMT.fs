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

module PoE.SMT

open System.Collections
open PoE.EvalUtils

type Z3Expr = Microsoft.Z3.BitVecExpr

type TranslationContext = {
  Z3Ctxt: Microsoft.Z3.Context
  True: Z3Expr
  False: Z3Expr
  SMTCache: Generic.Dictionary<string, Z3Expr>
}

let bvToZ3 ({ Z3Ctxt = z3 }) bv =
  z3.MkBV (BitVectorUtils.bvToBigInt bv |> string, uint32 bv.Length) :> Z3Expr

let private computeZ3Expr map ({ Z3Ctxt = z3 } as ctxt) (name: string) =
  match Map.find name map with
  | IntValue (n, t) -> z3.MkBV (n, uint32 (ExprType.toBitSize t)) :> Z3Expr
  | BitVecValue bv -> bvToZ3 ctxt bv
  | UnknownValue t -> z3.MkBVConst (name, uint32 (ExprType.toBitSize t))

let getZ3ExprFromVar map ctxt name =
  match ctxt.SMTCache.TryGetValue name with
  | true, e -> e
  | false, _ ->
    let e = computeZ3Expr map ctxt name
    ctxt.SMTCache.Add (name, e)
    e

let isSignedOp ltyp rtyp =
  match ltyp, rtyp with
  | TypeInt8, TypeInt8
  | TypeInt16, TypeInt16
  | TypeInt32, TypeInt32
  | TypeInt64, TypeInt64 -> true
  | _ -> false

let rec getType map = function
  | Var (name, _) ->
    match Map.find name map with
    | IntValue (_, t) -> t
    | BitVecValue bv -> TypeBV bv.Count
    | UnknownValue _ -> TypeAny
  | BitVector (bv, _) -> TypeBV bv.Count
  | Int (_, t, _) -> t
  | Unit -> TypeAny
  | BoolExpr _ -> TypeBV 1
  | ArithExpr (Neg (e, _)) -> getType map e
  | ArithExpr (Add (lhs, rhs, annot))
  | ArithExpr (Sub(lhs, rhs, annot))
  | ArithExpr (Mul (lhs, rhs, annot))
  | ArithExpr (Div (lhs, rhs, annot))
  | ArithExpr (Mod (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    TypeChecker.meetType annot ltyp rtyp
  | LogicExpr (Not (e, _)) -> getType map e
  | LogicExpr (Shl (lhs, rhs, annot))
  | LogicExpr (Shr (lhs, rhs, annot))
  | LogicExpr (And (lhs, rhs, annot))
  | LogicExpr (Or (lhs, rhs, annot))
  | LogicExpr (Xor (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    TypeChecker.meetType annot ltyp rtyp
  | TypeCast (t, _, _) -> t
  | Extract (_, _, _, _) -> TypeAnyBV
  | Concat (_, _, _) -> TypeAnyBV
  | Ite (_, texp, fexp, annot) ->
    let ltyp = getType map texp
    let rtyp = getType map fexp
    TypeChecker.meetType annot ltyp rtyp
  | _ -> failwith "Yet supported expression."

let getIntVal map = function
  | Var (name, _) ->
    match Map.find name map with
    | IntValue (v, t) -> uint32 v
    | _ -> failwith "Not an int."
  | Int (i, _, _) -> uint32 i
  | _ -> failwith "Not an int."

let rec toZ3 map ({ Z3Ctxt = z3 } as ctxt) annot = function
  | Var (name, _) -> getZ3ExprFromVar map ctxt name
  | BitVector (bv, _) -> bvToZ3 ctxt bv
  | Int (i, typ, _) -> z3.MkBV (i, uint32 (ExprType.toBitSize typ)) :> Z3Expr
  | Unit -> err annot "Cannot have Unit value for an SMT expression."
  | BoolExpr _ -> err annot "Cannot have BoolExpr for an SMT expression."
  | ArithExpr (Neg (e, _)) -> z3.MkBVNeg (toZ3 map ctxt annot e)
  | ArithExpr (Add (l, r, _)) ->
    z3.MkBVAdd (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | ArithExpr (Sub (l, r, _)) ->
    z3.MkBVSub (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | ArithExpr (Mul (l, r, _)) ->
    z3.MkBVMul (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | ArithExpr (Div (l, r, _)) ->
    let ltyp = getType map l
    let rtyp = getType map r
    if isSignedOp ltyp rtyp then
      z3.MkBVSDiv (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
    else z3.MkBVUDiv (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | ArithExpr (Mod (l, r, _)) ->
    let ltyp = getType map l
    let rtyp = getType map r
    if isSignedOp ltyp rtyp then
      z3.MkBVSRem (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
    else z3.MkBVURem (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (Shl (l, r, _)) ->
    z3.MkBVSHL (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (Shr (l, r, _)) ->
    let ltyp = getType map l
    let rtyp = getType map r
    if isSignedOp ltyp rtyp then
      z3.MkBVASHR (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
    else z3.MkBVLSHR (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (And (l, r, _)) ->
    z3.MkBVAND (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (Or (l, r, _)) ->
    z3.MkBVOR (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (Xor (l, r, _)) ->
    z3.MkBVXOR (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | LogicExpr (Not (e, _)) -> z3.MkBVNot (toZ3 map ctxt annot e)
  | Extract (e, soff, eoff, annot) ->
    let soff = getIntVal map soff
    let eoff = getIntVal map eoff
    z3.MkExtract (eoff, soff, toZ3 map ctxt annot e)
  | Concat (l, r, _) ->
    z3.MkConcat (toZ3 map ctxt annot l, toZ3 map ctxt annot r)
  | Ite (cond, texp, fexp, _) ->
    let cond = boolToZ3 map ctxt cond
    let t = toZ3 map ctxt annot texp
    let f = toZ3 map ctxt annot fexp
    z3.MkITE (cond, t, f) :?> Z3Expr
  | _ -> failwith "Yet supported expression."

and boolToZ3 map ({ Z3Ctxt = z3 } as ctxt) = function
  | BoolExpr (Eq (lhs, rhs, annot)) ->
    z3.MkEq (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
  | BoolExpr (Neq (lhs, rhs, annot)) ->
    z3.MkNot (z3.MkEq (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs))
  | BoolExpr (Gt (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    if isSignedOp ltyp rtyp then
      z3.MkBVSGT (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
    else z3.MkBVUGT (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
  | BoolExpr (Ge (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    if isSignedOp ltyp rtyp then
      z3.MkBVSGE (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
    else z3.MkBVUGE (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
  | BoolExpr (Lt (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    if isSignedOp ltyp rtyp then
      z3.MkBVSLT (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
    else z3.MkBVULT (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
  | BoolExpr (Le (lhs, rhs, annot)) ->
    let ltyp = getType map lhs
    let rtyp = getType map rhs
    if isSignedOp ltyp rtyp then
      z3.MkBVSLE (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
    else z3.MkBVULE (toZ3 map ctxt annot lhs, toZ3 map ctxt annot rhs)
  | _ -> err None "Invalid expression given for solve."

let evalConstDecl (m: Microsoft.Z3.Model) state (f: Microsoft.Z3.FuncDecl) =
  match m.ConstInterp f with
  | :? Microsoft.Z3.BitVecNum as n ->
    let symName = f.Name.ToString ()
    match findVar state symName None with
    | UnknownValue t ->
      if ExprType.isIntType t then
        let n = adjustIntByType n.Int64 t None
        addVar state symName (IntValue (n, t))
      else
        let bv = n.BigInteger.ToByteArray () |> BitVectorUtils.bytesToBV
        addVar state symName (BitVecValue bv)
    | _ -> err None "Known value is interpretered wrong."
  | _ -> err None "Failed to interpret FuncDecl."

let getAssignment state (m: Microsoft.Z3.Model) =
  m.ConstDecls |> Array.fold (evalConstDecl m) state

let query state { Z3Ctxt = z3 } boolExpr =
  let solver = z3.MkSolver ("QF_BV")
  solver.Assert ([| boolExpr |])
  match solver.Check () with
  | Microsoft.Z3.Status.SATISFIABLE -> getAssignment state solver.Model
  | _ -> err None "Unsatisfiable formula."

let solve expr state concvarMap =
  let d = Generic.Dictionary<string, string> (dict [ ("model", "true") ])
  let cache = Generic.Dictionary<string, Z3Expr> ()
  use z3 = new Microsoft.Z3.Context (d)
  let t = z3.MkBV (1, 1u)
  let f = z3.MkBV (0, 1u)
  let ctxt =
    { Z3Ctxt = z3
      True = t
      False = f
      SMTCache = cache }
  boolToZ3 concvarMap ctxt expr
  |> query state ctxt
