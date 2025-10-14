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

module TypeChecker =
  let rec assertTypeConsistency annot lhs rhs =
    match lhs, rhs with
    | TypeAny, _ | _, TypeAny -> ()
    | TypeAnyBV, TypeAnyBV | TypeAnyBV, TypeBV _ | TypeBV _, TypeAnyBV -> ()
    | TypeBV (ls), TypeBV (rs) when ls = rs -> ()
    | TypeInt8, TypeInt8 -> ()
    | TypeUInt8, TypeUInt8 -> ()
    | TypeInt16, TypeInt16 -> ()
    | TypeUInt16, TypeUInt16 -> ()
    | TypeInt32, TypeInt32 -> ()
    | TypeUInt32, TypeUInt32 -> ()
    | TypeInt64, TypeInt64 -> ()
    | TypeUInt64, TypeUInt64 -> ()
    | TypeAnyInt, TypeAnyInt -> ()
    | TypeAnyInt, TypeInt8 | TypeInt8, TypeAnyInt
    | TypeAnyInt, TypeUInt8 | TypeUInt8, TypeAnyInt
    | TypeAnyInt, TypeInt16 | TypeInt16, TypeAnyInt
    | TypeAnyInt, TypeUInt16 | TypeUInt16, TypeAnyInt
    | TypeAnyInt, TypeInt32 | TypeInt32, TypeAnyInt
    | TypeAnyInt, TypeUInt32 | TypeUInt32, TypeAnyInt
    | TypeAnyInt, TypeInt64 | TypeInt64, TypeAnyInt
    | TypeAnyInt, TypeUInt64 | TypeUInt64, TypeAnyInt -> ()
    | TypeUnit, TypeUnit -> ()
    | TypeFunction (lret, largs), TypeFunction (rret, rargs) ->
      assertTypeConsistency annot lret rret
      List.iter2 (fun l r -> assertTypeConsistency annot l r) largs rargs
    | _ -> TypeEnvironment.Err annot ("Type mismatch ("
                                     + ExprType.toString lhs
                                     + " vs. "
                                     + ExprType.toString rhs + ")")

  let meetType annot lhs rhs =
    assertTypeConsistency annot lhs rhs
    match lhs, rhs with
    | TypeAnyBV, _ | TypeAnyInt, _ | TypeAny, _ -> rhs
    | _ -> lhs

  let computeExtractType t soff eoff annot =
    if ExprType.isIntType soff && ExprType.isIntType eoff then TypeAnyBV
    else TypeEnvironment.Err annot "Extraction offset needs to be an integer."

  let computeFuncType func args annot =
    match func with
    | TypeFunction (expectedRet, expectedArgs) ->
      if List.length expectedArgs = List.length args then
        List.iter2 (assertTypeConsistency annot) expectedArgs args
      else () (* Loose check. *)
      expectedRet
    | _ -> TypeEnvironment.Err annot "Invalid function type."

  let assertCond annot = function
    | TypeBV 1 | TypeAny | TypeAnyBV -> ()
    | _ -> TypeEnvironment.Err annot "Invalid condition type used."

  let inline makeSpecificBVType sz = TypeBV sz

  let checkSubstrOffsets annot sOff eOff mysz =
    if sOff > 0L && eOff = -1L then
      makeSpecificBVType (mysz - Convert.ToInt32 sOff)
    elif eOff = -1L then
      makeSpecificBVType (Convert.ToInt32 (- sOff))
    else
      let len = eOff - sOff + 1L |> Convert.ToInt32
      if len <= mysz then makeSpecificBVType len
      else TypeEnvironment.Err annot "Invalid offset given."

  let concatTypes annot lhs rhs =
    match lhs, rhs with
    | TypeBV lsz, TypeBV rsz ->
      TypeBV (lsz + rsz)
    | TypeBV _, TypeAnyBV
    | TypeAnyBV, TypeBV _
    | TypeAnyBV, TypeAnyBV ->
      TypeAnyBV
    | TypeBV sz, TypeInt8
    | TypeBV sz, TypeUInt8
    | TypeInt8, TypeBV sz
    | TypeUInt8, TypeBV sz ->
      TypeBV (8 + sz)
    | TypeBV sz, TypeInt16
    | TypeBV sz, TypeUInt16
    | TypeInt16, TypeBV sz
    | TypeUInt16, TypeBV sz ->
      TypeBV (16 + sz)
    | TypeBV sz, TypeInt32
    | TypeBV sz, TypeUInt32
    | TypeInt32, TypeBV sz
    | TypeUInt32, TypeBV sz ->
      TypeBV (32 + sz)
    | TypeBV sz, TypeInt64
    | TypeBV sz, TypeUInt64
    | TypeInt64, TypeBV sz
    | TypeUInt64, TypeBV sz ->
      TypeBV (64 + sz)
    | _ ->
      if ExprType.isConcreteIntType lhs && ExprType.isConcreteIntType rhs then
        TypeBV (ExprType.toBitSize lhs + ExprType.toBitSize rhs)
      else
        TypeAnyBV

  let assertReplacements annot types =
    if types |> List.forall ExprType.isBVType then ()
    else TypeEnvironment.Err annot "Replacements must be a BitVector."

  let rec computeExprType (env: TypeEnvironment) = function
    | Var (name, annot) -> env.FindSymbolType name annot
    | BitVector (bv, _) -> makeSpecificBVType bv.Length
    | Int (_, typ, _) -> typ
    | Unit -> TypeUnit
    | BoolExpr (boolExpr) -> computeBoolExprType env boolExpr
    | ArithExpr (arithExpr) -> computeArithExprType env arithExpr
    | LogicExpr (logicExpr) -> computeLogicExprType env logicExpr
    | TypeCast (TypeAnyBV, e, annot) ->
      let etype = computeExprType env e
      if isCompatible env e
        && (ExprType.isConcreteValueType etype
          || etype = TypeAnyBV || etype = TypeAny) then
        TypeAnyBV
      else TypeEnvironment.Err annot "Invalid type cast."
    | TypeCast (t, e, annot) ->
      if isCompatible env e && ExprType.isConcreteValueType t then t
      else TypeEnvironment.Err annot "Invalid type cast."
    | Extract (e, soff, eoff, annot) ->
      let t = computeExprType env e
      let soff = computeExprType env soff
      let eoff = computeExprType env eoff
      computeExtractType t soff eoff annot
    | Concat (lhs, rhs, annot) ->
      let ltype = computeExprType env lhs
      let rtype = computeExprType env rhs
      concatTypes annot ltype rtype
    | BVMult (v, _, annot) ->
      let vtype = computeExprType env v
      if ExprType.isBVType vtype || vtype = TypeAny then TypeAnyBV
      else TypeEnvironment.Err annot "Invalid multiplied bitvector."
    | Ite (cond, texpr, fexpr, annot) ->
      computeExprType env cond |> assertCond annot
      let ttype = computeExprType env texpr
      let ftype = computeExprType env fexpr
      assertTypeConsistency annot ttype ftype
      ttype
    | Call (name, args, annot) ->
      let funcType = env.FindSymbolType name annot
      let argsType = List.map (computeExprType env) args
      computeFuncType funcType argsType annot
    | AsmExpr (_, asm, replacements, annot) ->
      replacements |> List.map (computeExprType env) |> assertReplacements annot
      let numPlaceholders = ASTUtils.countAsmPlaceholders asm
      let numReplacements = List.length replacements
      if numPlaceholders = numReplacements then TypeAnyBV
      elif numPlaceholders > numReplacements then
        TypeEnvironment.Err annot "More replacements required for AsmExpr."
      else TypeEnvironment.Err annot "Not enough replacements for AsmExpr."

  and computeBoolExprType env = function
    | Eq (lhs, rhs, annot)
    | Neq (lhs, rhs, annot) ->
      let ltype = computeExprType env lhs
      let rtype = computeExprType env rhs
      assertTypeConsistency annot ltype rtype
      TypeBV 1
    | Gt (lhs, rhs, annot)
    | Ge (lhs, rhs, annot)
    | Lt (lhs, rhs, annot)
    | Le (lhs, rhs, annot) ->
      let ltype = computeExprType env lhs
      let rtype = computeExprType env rhs
      assertTypeConsistency annot ltype rtype
      if ExprType.isIntType ltype && ExprType.isIntType rtype then TypeBV 1
      else TypeEnvironment.Err annot "Comparison must be done with integers."

  and computeArithExprType env = function
    | Neg (e, _)
    | BNot (e, _) ->
      computeExprType env e
    | Add (lhs, rhs, annot)
    | Sub (lhs, rhs, annot)
    | Mul (lhs, rhs, annot)
    | Div (lhs, rhs, annot)
    | Mod (lhs, rhs, annot)
    | BAnd (lhs, rhs, annot)
    | BOr (lhs, rhs, annot)
    | BXor (lhs, rhs, annot) ->
      let ltype = computeExprType env lhs
      let rtype = computeExprType env rhs
      meetType annot ltype rtype

  and computeLogicExprType env = function
    | Not (e, _) -> computeExprType env e
    | Shl (lhs, rhs, annot)
    | Shr (lhs, rhs, annot)
    | And (lhs, rhs, annot)
    | Or (lhs, rhs, annot)
    | Xor (lhs, rhs, annot) ->
      let ltype = computeExprType env lhs
      let rtype = computeExprType env rhs
      meetType annot ltype rtype

  and isCompatible env expr =
    match computeExprType env expr with
    | TypeUnit -> false
    | _ -> true

  let checkFunctionArgs (env: TypeEnvironment) name args annot =
    let iterVar = function
      | Var (n, annot), typ -> env.AddSymbolType n typ annot
      | _ -> TypeEnvironment.Err annot
               ("Function (" + name + ") has invalid argument(s).")
    List.iter iterVar args

  let createFuncType retType args =
    if List.length args = 0 then TypeFunction (retType, [])
    else TypeFunction (retType, List.map (fun _ -> TypeAny) args)

  let checkVarDecls (env: TypeEnvironment) vTyp vars annot =
    vars
    |> List.iter (fun (name, expr) ->
      match expr with
      | Unit -> (* Uninitialized var. *) env.AddSymbolType name vTyp annot
      | _ ->
        let expr = TypeCast (vTyp, expr, annot) (* Implicit casting. *)
        let ty = computeExprType env expr
        let ty = meetType annot vTyp ty
        env.AddSymbolType name ty annot)

  let computeTypesOfExprs env exprs =
    exprs
    |> List.map (computeExprType env)

  let checkFuncCall (env: TypeEnvironment) name args annot =
    match env.FindSymbolType name annot with
    | TypeFunction (_retType, knownArgTypes) ->
      let inputArgTypes = computeTypesOfExprs env args
      List.iter2 (assertTypeConsistency annot) knownArgTypes inputArgTypes
    | _ -> TypeEnvironment.Err annot "Invalid function type."

  let rec checkBlock env name = function
    | VarStmt (vTyp, vars, annot) :: next ->
      checkVarDecls env vTyp vars annot
      checkBlock env name next
    | CallStmt (name, args, annot) :: next ->
      checkFuncCall env name args annot
      checkBlock env name next
    | Return (e, _annot) :: next ->
      computeExprType env e |> ignore
      checkBlock env name next
    | AssignStmt (var, e, annot) :: next ->
      let etyp = computeExprType env e
      let vtyp = env.FindSymbolType var annot
      assertTypeConsistency annot etyp vtyp
      checkBlock env name next
    | ForStmt (vname, init, fini, body, annot) :: next ->
      let ityp = computeExprType env init
      let ftyp = computeExprType env fini
      assertTypeConsistency annot ityp ftyp
      env.AddSymbolType vname ityp annot
      env.EnterScope ()
      checkBlock env name body
      env.ExitScope ()
      checkBlock env name next
    | WhileStmt (cond, body, annot) :: next ->
      computeExprType env cond |> assertCond annot
      env.EnterScope ()
      checkBlock env name body
      env.ExitScope ()
      checkBlock env name next
    | CondStmt (cond, tbody, fbody, annot) :: next ->
      computeExprType env cond |> assertCond annot
      checkBlock env name tbody
      checkBlock env name fbody
      checkBlock env name next
    | BreakStmt (_) :: next ->
      checkBlock env name next
    | NopStmt (_):: next ->
      checkBlock env name next
    | SolveStmt (e, annot) :: next ->
      computeExprType env e |> assertCond annot
      checkBlock env name next
    | DebugStmt (e, _, _annot) :: next ->
      computeExprType env e |> ignore
      checkBlock env name next
    | [] -> ()

  let rec checkBlockReturning name annot block =
    if checkBlockReturningAux name annot block then ()
    else TypeEnvironment.Err annot $"{name} does not return."

  and checkBlockReturningAux name annot = function
    | [] -> false
    | Return (_, _) :: _ -> true
    | CondStmt (_, tbody, fbody, annot) :: tl ->
      if checkBlockReturningAux name annot tbody
         && checkBlockReturningAux name annot fbody then true
      else checkBlockReturningAux name annot tl
    | _ :: tl -> checkBlockReturningAux name annot tl

  let [<Literal>] private submit = "submit"

  let rec checkDecls submitCnt (env: TypeEnvironment) = function
    | GlobalVarDecl (vTyp, vars, annot) :: next ->
      checkVarDecls env vTyp vars annot
      checkDecls submitCnt env next
    | FunDecl (name, args, body, annot) :: next
    | ActDecl (name, args, body, annot) :: next ->
      env.AddSymbolType name (createFuncType TypeAny args) annot
      env.EnterScope ()
      checkFunctionArgs env name args annot
      checkBlock env name body
      checkBlockReturning name annot body
      env.ExitScope ()
      checkDecls submitCnt env next
    | SubmitDecl (body, annot) :: next ->
      env.AddSymbolType submit (createFuncType TypeAny []) annot
      env.EnterScope ()
      checkBlock env submit body
      checkBlockReturning submit annot body
      env.ExitScope ()
      checkDecls (submitCnt + 1) env next
    | [] -> submitCnt

  let check poe =
    let funcs =
      BuiltIns.functions
      |> Array.map (fun fn -> fn.Name, fn.ReturnType, fn.ParamTypes)
    let env = TypeEnvironment (funcs)
    let submissionCnt = checkDecls 0 env poe.Decls
    if submissionCnt <> 1 then
      TypeEnvironment.Err None "PoE should contain exactly one submit stmt."
    else poe, env
