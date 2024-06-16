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

module PoE.Executor

open System
open System.Collections
open B2R2
open B2R2.Peripheral.Assembly
open PoE.StringUtils
open PoE.Stream
open PoE.EvalUtils

let toBV annot = function
  | IntValue (n, t) -> BitVectorUtils.intToBV n (ExprType.toBitSize t)
  | BitVecValue bv -> bv
  | _ -> err annot "Invalid value used for bitvector operation."

let recoverSign (n: int64) (t: ExprType) =
  match t with
  | TypeInt8 -> int8 n |> int64
  | TypeInt16 -> int16 n |> int64
  | TypeInt32 -> int32 n |> int64
  | _ -> n

let computeRuntimeType = function
  | IntValue (_, t) -> t
  | BitVecValue (bv) -> TypeBV bv.Count
  | UnknownValue _ -> TypeAny

let updateIntType v targetType =
  match v with
  | IntValue (n, _) -> IntValue (n, targetType)
  | _ -> err None "Fatal error: not an int type."

let initFunctionMap () =
  BuiltIns.functions
  |> Array.fold (fun acc fn ->
    let typ = TypeFunction (fn.ReturnType, fn.ParamTypes)
    Map.add fn.Name (BuiltinFunction (typ, fn)) acc) Map.empty

let incPC state =
  { state with ProgCounter = state.ProgCounter + 1 }

let isEqual v1 v2 =
  match v1, v2 with
  | BitVecValue bv1, BitVecValue bv2 -> BitVectorUtils.isBVEQ bv1 bv2
  | IntValue (i1, TypeAnyInt), IntValue (i2, _)
  | IntValue (i1, _), IntValue (i2, TypeAnyInt) -> i1 = i2
  | IntValue (i1, t1), IntValue (i2, t2) -> t1 = t2 && i1 = i2
  | _, _ -> false

let assertValidInt n t annot =
  if (ExprType.ofInt64 n |> ExprType.toBitSize) <= ExprType.toBitSize t then ()
  else err annot "Invalid integer value is given."

let toSignedNum annot = function
  | IntValue (n, t) ->
    assertValidInt n t annot
    struct (recoverSign n t, t)
  | _ -> err annot "Invalid value used for arithmetic."

let toUnsignedNum annot = function
  | IntValue (n, t) ->
    assertValidInt n t annot
    struct (uint64 n, t)
  | _ -> err annot "Invalid value used for arithmetic."

let private prepareBinOp v1 v2 annot =
  let struct (n1, t1) = toSignedNum annot v1
  let struct (n2, t2) = toSignedNum annot v2
  if t1 <> t2 then err annot "Type mismatch for binop."
  else struct (n1, n2, t1)

let private prepareUnsignedBinOp v1 v2 annot =
  let struct (n1, t1) = toUnsignedNum annot v1
  let struct (n2, t2) = toUnsignedNum annot v2
  if t1 <> t2 then err annot "Type mismatch for binop."
  else struct (n1, n2, t1)

let signedComp v1 v2 op annot =
  let struct (n1, n2, _) = prepareBinOp v1 v2 annot
  op n1 n2

let unsignedComp v1 v2 op annot =
  let struct (n1, n2, _) = prepareUnsignedBinOp v1 v2 annot
  op n1 n2

let signedArith v1 v2 op annot =
  let struct (n1, n2, t) = prepareBinOp v1 v2 annot
  IntValue (adjustIntByType (op n1 n2) t annot, t)

let unsignedArith v1 v2 op annot =
  let struct (n1, n2, t) = prepareUnsignedBinOp v1 v2 annot
  let n = op (uint64 n1) (uint64 n2) |> int64
  IntValue (adjustIntByType n t annot, t)

let prepareIntOperands lhs rhs annot =
  match lhs, rhs with
  | IntValue (ln, lt), IntValue (rn, rt) ->
    TypeChecker.assertTypeConsistency annot lt rt
    let ty = TypeChecker.meetType annot lt rt
    IntValue (adjustIntByType ln ty annot, ty),
    IntValue (adjustIntByType rn ty annot, ty)
  | _ -> err annot (sprintf "Type mismatch (%A vs. %A)" lhs rhs)

let trueValue = BitVecValue (BitArray ([| true |]))

let falseValue = BitVecValue (BitArray ([| false |]))

let eq lhs rhs = if isEqual lhs rhs then trueValue else falseValue

let neq lhs rhs = if isEqual lhs rhs then falseValue else trueValue

let gt v1 v2 annot =
  if unsignedComp v1 v2 ( > ) annot then trueValue else falseValue

let sgt v1 v2 annot =
  if signedComp v1 v2 ( > ) annot then trueValue else falseValue

let ge v1 v2 annot =
  if unsignedComp v1 v2 ( >= ) annot then trueValue else falseValue

let sge v1 v2 annot =
  if signedComp v1 v2 ( >= ) annot then trueValue else falseValue

let lt v1 v2 annot =
  if unsignedComp v1 v2 ( < ) annot then trueValue else falseValue

let slt v1 v2 annot =
  if signedComp v1 v2 ( < ) annot then trueValue else falseValue

let le v1 v2 annot =
  if unsignedComp v1 v2 ( <= ) annot then trueValue else falseValue

let sle v1 v2 annot =
  if signedComp v1 v2 ( <= ) annot then trueValue else falseValue

let add v1 v2 annot = signedArith v1 v2 ( + ) annot

let sub v1 v2 annot = signedArith v1 v2 ( - ) annot

let mul v1 v2 annot = signedArith v1 v2 ( * ) annot

let div v1 v2 annot = unsignedArith v1 v2 ( / ) annot

let sdiv v1 v2 annot = signedArith v1 v2 ( / ) annot

let rem v1 v2 annot = unsignedArith v1 v2 ( % ) annot

let srem v1 v2 annot = signedArith v1 v2 ( % ) annot

let neg v annot =
  let struct (n, t) = toSignedNum annot v
  if ExprType.isIntType t then IntValue (adjustIntByType (- n) t annot, t)
  else err annot "Invalid value for negation."

let shl v1 v2 annot =
  let struct (n1, n2, t) = prepareBinOp v1 v2 annot
  let n = n1 <<< (Convert.ToInt32 n2)
  IntValue (adjustIntByType n t annot, t)

let shr v1 v2 annot =
  let struct (n1, n2, t) = prepareUnsignedBinOp v1 v2 annot
  let n = (n1 >>> (Convert.ToInt32 n2)) |> int64
  IntValue (adjustIntByType n t annot, t)

let ashr v1 v2 annot =
  let struct (n1, n2, t) = prepareBinOp v1 v2 annot
  let n = n1 >>> (Convert.ToInt32 n2)
  IntValue (adjustIntByType n t annot, t)

let logand v1 v2 annot = signedArith v1 v2 ( &&& ) annot

let logor v1 v2 annot = signedArith v1 v2 (|||) annot

let logxor v1 v2 annot = signedArith v1 v2 (^^^) annot

let lognot v annot =
  let struct (n, t) = toSignedNum annot v
  IntValue (adjustIntByType (~~~ n) t annot, t)

let getmask count annot =
  let rec loop mask count =
    if count > 0 then loop ((mask <<< 1) + 1L) (count - 1)
    else mask
  if count = 0 then err annot "Invalid conversion."
  else loop 1L (count - 1)

let rec extract sOff eOff v annot =
  match v with
  | BitVecValue bv ->
    if sOff >= 0 && eOff < 0 && (eOff - sOff + 1 <= 0) then
      BitVectorUtils.subBV bv sOff (bv.Length + eOff - sOff + 1) |> BitVecValue
    elif sOff < 0 && eOff < 0 && (eOff >= sOff) then
      BitVectorUtils.subBV bv (bv.Length + sOff) (eOff - sOff + 1)
      |> BitVecValue
    elif sOff >= 0 && eOff >= 0 then
      BitVectorUtils.subBV bv sOff (eOff - sOff + 1) |> BitVecValue
    else err annot "Invalid extract."
  | IntValue (n, t) ->
    let bv = BitVectorUtils.intToBV n (ExprType.toBitSize t)
    extract sOff eOff (BitVecValue bv) annot
  | _ -> err annot "Invalid value used for extract."

let popValue state annot =
  match state.RetStack with
  | [] -> err annot "Invalid calling context."
  | hd :: tl -> { state with RetStack = tl }, hd

let pushValue state v =
  { state with RetStack = v :: state.RetStack }

let returnValue state v =
  let state = pushValue state v
  { state with ProgCounter = -1; IsReturning = true }

let submit state v annot =
  let myflag = toBV annot v |> BitVectorUtils.bvToBytes |> bytesToStr
  let state = { state with SubmittedFlag = Some myflag }
  match state.FlagPath with
  | Some path ->
    let givenflag = IO.File.ReadAllText (path)
    let success = givenflag = myflag
    if success then printer.PrintLine "Success."
    else printer.PrintLine ("Given flag: " + givenflag + "; My flag: " + myflag)
    pushValue state (if success then trueValue else falseValue)
  | None ->
    printer.PrintLine ("FLAG = [" + myflag + "]")
    state

let chooseRightBinOp lhs rhs sop uop annot =
  match lhs, rhs with
  | IntValue (_, TypeInt8), IntValue (_, TypeInt8)
  | IntValue (_, TypeInt16), IntValue (_, TypeInt16)
  | IntValue (_, TypeInt32), IntValue (_, TypeInt32)
  | IntValue (_, TypeInt64), IntValue (_, TypeInt64)
  | IntValue (_, TypeAnyInt), IntValue (_, TypeAnyInt) -> sop
  | IntValue (_, TypeUInt8), IntValue (_, TypeUInt8)
  | IntValue (_, TypeUInt16), IntValue (_, TypeUInt16)
  | IntValue (_, TypeUInt32), IntValue (_, TypeUInt32)
  | IntValue (_, TypeUInt64), IntValue (_, TypeUInt64) -> uop
  | _ -> err annot "Binop type mismatch."

let rec obtainVars state expr annot map =
  match expr with
  | Var (name, annot) -> Map.add name (findVar state name annot) map
  | BitVector (_, _) -> map
  | Int (_, _, _) -> map
  | Unit -> err annot "Unexpected expression encountered."
  | BoolExpr (Eq (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BoolExpr (Neq (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BoolExpr (Gt (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BoolExpr (Ge (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BoolExpr (Lt (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BoolExpr (Le (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | ArithExpr (Neg (e, annot)) -> obtainVars state e annot map
  | ArithExpr (Add (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | ArithExpr (Sub (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | ArithExpr (Mul (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | ArithExpr (Div (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | ArithExpr (Mod (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (Shl (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (Shr (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (And (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (Or (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (Xor (lhs, rhs, annot)) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | LogicExpr (Not (e, annot)) -> obtainVars state e annot map
  | TypeCast (_, e, annot) -> obtainVars state e annot map
  | Extract (e, soff, eoff, annot) ->
    obtainVars state e annot map
    |> obtainVars state soff annot
    |> obtainVars state eoff annot
  | Concat (lhs, rhs, annot) ->
    obtainVars state lhs annot map |> obtainVars state rhs annot
  | BVMult (v, multiplier, annot) ->
    obtainVars state v annot map |> obtainVars state multiplier annot
  | Ite (cond, texp, fexp, annot) ->
    obtainVars state cond annot map
    |> obtainVars state texp annot
    |> obtainVars state fexp annot
  | Call (_, args, annot) ->
    args |> List.fold (fun map e -> obtainVars state e annot map) map
  | AsmExpr (_, _, _, _) -> map

let rec evalExpr state expr annot =
  match expr with
  | Var (name, annot) -> state, findVar state name annot
  | BitVector (bv, _) -> state, BitVecValue bv
  | Int (n, t, _) -> state, IntValue (n, t)
  | Unit -> state, UnknownValue TypeUnit
  | BoolExpr (e) -> evalBoolExpr state e
  | ArithExpr (e) -> evalArithExpr state e
  | LogicExpr (e) -> evalLogicExpr state e
  | TypeCast (t, e, annot) -> evalTypeCast state t e annot
  | Extract (e, soff, eoff, annot) -> evalExtract state e soff eoff annot
  | Concat (lhs, rhs, _) -> evalConcat state lhs rhs annot
  | BVMult (v, multiplier, annot) -> evalBVMult state v multiplier annot
  | Ite (cond, texp, fexp, annot) -> evalIte state cond texp fexp annot
  | Call (fname, arg, annot) -> evalCall state fname arg annot
  | AsmExpr (arch, asm, replacements, annot) ->
    let bs, state = evalAsm state arch asm replacements annot
    state, BitVecValue (BitVectorUtils.bytesToBV bs)

and evalBinOpNoTypecheck state lhs rhs op annot =
  let state, lhs = evalExpr state lhs annot
  let state, rhs = evalExpr state rhs annot
  state, op lhs rhs

and evalBinOpTypeCheck state lhs rhs op annot =
  let state, lhs = evalExpr state lhs annot
  let state, rhs = evalExpr state rhs annot
  let lhs, rhs = prepareIntOperands lhs rhs annot
  state, op lhs rhs annot

and evalAdaptiveIntBinOp state lhs rhs sop uop annot =
  let state, lhs = evalExpr state lhs annot
  let state, rhs = evalExpr state rhs annot
  let lhs, rhs = prepareIntOperands lhs rhs annot
  let op = chooseRightBinOp lhs rhs sop uop annot
  state, op lhs rhs annot

and evalBoolExpr state = function
  | Eq (lhs, rhs, annot) -> evalBinOpNoTypecheck state lhs rhs eq annot
  | Neq (lhs, rhs, annot) -> evalBinOpNoTypecheck state lhs rhs neq annot
  | Gt (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs sgt gt annot
  | Ge (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs sge ge annot
  | Lt (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs slt lt annot
  | Le (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs sle le annot

and evalArithExpr state = function
  | Neg (e, annot) ->
    let state, v = evalExpr state e annot in state, neg v annot
  | Add (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs add annot
  | Sub (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs sub annot
  | Mul (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs mul annot
  | Div (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs sdiv div annot
  | Mod (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs srem rem annot

and evalLogicExpr state = function
  | Shl (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs shl annot
  | Shr (lhs, rhs, annot) -> evalAdaptiveIntBinOp state lhs rhs ashr shr annot
  | And (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs logand annot
  | Or (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs logor annot
  | Xor (lhs, rhs, annot) -> evalBinOpTypeCheck state lhs rhs logxor annot
  | Not (e, annot) ->
    let state, v = evalExpr state e annot in state, lognot v annot

and evalTypeCast state targetType e annot =
  let state, v = evalExpr state e annot
  match v with
  | BitVecValue bv ->
    match targetType with
    | TypeAnyBV -> state, v
    | TypeInt8
    | TypeInt16
    | TypeInt32
    | TypeInt64 -> state, intValue targetType bv true
    | TypeUInt8
    | TypeUInt16
    | TypeUInt32
    | TypeUInt64 -> state, intValue targetType bv false
    | _ -> err annot "Invalid casting of bitvector."
  | IntValue (n, t) ->
    match targetType with
    | TypeAnyBV -> state, bvValue t n
    | TypeInt8 -> state, IntValue (n &&& 0xffL, targetType)
    | TypeUInt8 -> state, IntValue (n &&& 0xffL, targetType)
    | TypeInt16 -> state, IntValue (recoverSign n t &&& 0xffffL, targetType)
    | TypeUInt16 -> state, IntValue (n &&& 0xffffL, targetType)
    | TypeInt32 -> state, IntValue (recoverSign n t &&& 0xffffffffL, targetType)
    | TypeUInt32 -> state, IntValue (n &&& 0xffffffffL, targetType)
    | TypeInt64 -> state, IntValue (recoverSign n t, targetType)
    | TypeUInt64 -> state, IntValue (n, targetType)
    | _ -> err annot "Invalid casting of integer."
  | UnknownValue _ -> err annot "Cannot cast unknowns."

and evalExtract state e soff eoff annot =
  let state, v = evalExpr state e annot
  let state, soff = evalExpr state soff annot
  let state, eoff = evalExpr state eoff annot
  match soff, eoff with
  | IntValue (soff, _), IntValue (eoff, _) ->
    state, extract (int soff) (int eoff) v annot
  | _ -> err annot "Invalid extract offset."

and evalConcat state lhs rhs annot =
  let state, lhs = evalExpr state lhs annot
  let state, rhs = evalExpr state rhs annot
  match lhs, rhs with
  | UnknownValue _, _ ->
    state, BitVecValue (toBV annot rhs) (* unit + rhs = rhs *)
  | _, UnknownValue _ ->
    state, BitVecValue (toBV annot lhs) (* lhs + unit = lhs *)
  | _ ->
    state,
    BitVectorUtils.concatBV (toBV annot lhs) (toBV annot rhs) |> BitVecValue

and evalBVMult state v multiplier annot =
  let state, v = evalExpr state v annot
  let state, multiplier = evalExpr state multiplier annot
  let struct (multiplier, _) = toUnsignedNum annot multiplier
  let bv = BitVectorUtils.multiplyBV (toBV annot v) (int multiplier)
  state, BitVecValue bv

and evalIte state cond texp fexp annot =
  let state, cond = evalExpr state cond annot
  match cond with
  | BitVecValue _ ->
    if isEqual trueValue cond then evalExpr state texp annot
    else evalExpr state fexp annot
  | _ -> err annot "Invalid condition for ite."

and updateFuncArg annot state vardef value =
  let state, v = evalExpr state value annot
  addVar state vardef v

and prepareArgs state argnames argvals annot =
  List.fold2 (updateFuncArg annot) state argnames argvals

and callAndReturn state argnames argvals body annot =
  let state = prepareArgs state argnames argvals annot
  let oldProg = state.Program
  let oldPC = state.ProgCounter
  let state = evalProgram { state with Program = body; ProgCounter = 0 }
  let isReturning = state.IsReturning
  if not isReturning then err annot "No return found."
  else
    let state =
      { state with Program = oldProg; ProgCounter = oldPC; IsReturning = false }
    popValue state annot

and evalCall state fname argvals annot =
  let oldVars = state.VarMap
  match findFunction state fname annot with
  | ActionFunction (_, argnames, body) ->
    if EvalUtils.isAct state then err annot "Nested acts are not supported."
    else
      let stream = createStream state
      let state = { state with Stream = Some stream }
      let state, ret = callAndReturn state argnames argvals body annot
      stream.Close ()
      { state with VarMap = oldVars; Stream = None }, ret
  | RegularFunction (_, argnames, body) ->
    let state, ret = callAndReturn state argnames argvals body annot
    { state with VarMap = oldVars }, ret
  | BuiltinFunction (_, fn) ->
    let state, argvals =
      argvals |> List.fold (fun (state, argvals) arg ->
        let state, v = evalExpr state arg annot
        state, v :: argvals
      ) (state, [])
    fn.Execute state (List.rev argvals) annot

and evalReplacements acc state replacements annot =
  match replacements with
  | replacement :: tl ->
    let state, v = evalExpr state replacement annot
    let acc = (toBV annot v |> BitVectorUtils.bvToBytes |> bytesToStr) :: acc
    evalReplacements acc state tl annot
  | [] -> List.rev acc, state

and convertChars str =
  String.explode str
  |> List.map (fun ch ->
    if ch = ASTUtils.asmVarPlaceholder then None else Some ch)

and popReplacement annot = function
  | hd :: tl -> hd, tl
  | [] -> raise (ParsingException ("Assembly variable count mismatch.", annot))

and substituteAsmArgs acc replacements annot = function
  | Some ch :: rest -> substituteAsmArgs (ch :: acc) replacements annot rest
  | None :: rest ->
    let r, replacements = popReplacement annot replacements
    let acc = (String.explode r |> List.rev) @ acc
    substituteAsmArgs acc replacements annot rest
  | [] -> List.rev acc |> List.toArray |> String

and evalAsm state archStr asmStr replacements annot =
  let isa = ISA.OfString archStr
  let asm = AsmInterface (isa, 0UL)
  let chars = convertChars asmStr
  let replacements, state = evalReplacements [] state replacements annot
  let s = substituteAsmArgs [] replacements annot chars
  match asm.AssembleBin s with
  | Ok lst -> Array.concat lst, state
  | Error e -> raise (ParsingException (e, annot))

and evalVars state vType decls annot =
  match decls with
  | (name, Unit) :: rest ->
    let state = addVar state name (UnknownValue vType)
    evalVars state vType rest annot
  | (name, e) :: rest ->
    let e = TypeCast (vType, e, annot) (* Implicit casting. *)
    let state, v = evalExpr state e annot
    let state = addVar state name v
    evalVars state vType rest annot
  | [] -> state

and evalForLoop state loopvar fini body annot =
  let v = findVar state loopvar annot
  let state, loopcond = evalBoolExpr state (Le (toASTInt annot v, fini, annot))
  if state.IsReturning then state
  elif loopcond = trueValue && not state.IsBreaking then
    let state = evalProgram { state with Program = body; ProgCounter = 0 }
    let one = Int (1L, TypeUInt64, annot)
    let state, v' = evalArithExpr state (Add (toASTInt annot v, one, annot))
    let state' = addVar state loopvar v'
    evalForLoop state' loopvar fini body annot
  elif state.IsBreaking then { state with IsBreaking = false }
  else state

and evalWhileLoop state condExpr body annot =
  let state, cond = evalExpr state condExpr annot
  if state.IsReturning then state
  elif cond = trueValue && not state.IsBreaking then
    let state' = evalProgram { state with Program = body; ProgCounter = 0 }
    evalWhileLoop state' condExpr body annot
  elif state.IsBreaking then
    { state with IsBreaking = false }
  else
    state

and pushVarIntoScope state v init =
  match Map.tryFind v state.VarMap with
  | Some oldV ->
    addVar state v init,
    fun state -> { state with VarMap = Map.add v oldV state.VarMap }
  | None ->
    addVar state v init,
    fun state -> { state with VarMap = Map.remove v state.VarMap }

and evalStmt state stmt =
  match state.StreamErrorSite with
  | Some annot -> evalStmtError state stmt annot
  | None -> evalStmtRegular state stmt

and evalStmtError state stmt annot =
  if EvalUtils.isAct state then
    BitArray 0
    |> BitVecValue
    |> returnValue state
  else 
    let state = { state with StreamErrorSite = None }
    let site = match annot with Some i -> i.ToString () | None -> "-"
    let msg = "A stream was disconnected @ " + site
    let colored =
      [ (NoColor, "[")
        (Yellow, "WARN")
        (NoColor, "] ")
        (Red, msg) ]
    printer.PrintLine colored
    evalStmt state stmt

and evalStmtRegular state = function
  | VarStmt (vType, decls, annot) ->
    evalVars state vType decls annot
    |> incPC
  | CallStmt (fname, args, annot) ->
    let state, _ = evalCall state fname args annot
    incPC state
  | Return (e, annot) -> evalExpr state e annot ||> returnValue
  | AssignStmt (vname, e, annot) ->
    let state, v = evalExpr state e annot
    let origType = findVar state vname annot |> computeRuntimeType
    let v =
      if ExprType.isIntType origType then
        let currType = computeRuntimeType v
        let targetType = TypeChecker.meetType annot origType currType
        updateIntType v targetType
      else v
    let state = addVar state vname v
    incPC state
  | ForStmt (v, init, fini, body, annot) ->
    let oldprog = state.Program
    let oldpc = state.ProgCounter
    let state, init = evalExpr state (TypeCast (TypeUInt64, init, annot)) annot
    let state, fini = evalExpr state (TypeCast (TypeUInt64, fini, annot)) annot
    let fini = toASTInt annot fini
    let state, popFn = pushVarIntoScope state v init
    let state = evalForLoop state v fini (List.toArray body) annot
    let state = popFn state
    if state.IsReturning then state
    else { state with Program = oldprog; ProgCounter = oldpc } |> incPC
  | WhileStmt (cond, body, annot) ->
    let oldprog = state.Program
    let oldpc = state.ProgCounter
    let state = evalWhileLoop state cond (List.toArray body) annot
    if state.IsReturning then state
    else { state with Program = oldprog; ProgCounter = oldpc } |> incPC
  | CondStmt (cond, tbody, fbody, annot) ->
    let state, cond = evalExpr state cond annot
    let oldprog = state.Program
    let oldpc = state.ProgCounter
    if cond = trueValue then
      let state =
        evalProgram { state with Program = List.toArray tbody; ProgCounter = 0 }
      if state.IsReturning then state
      else incPC { state with Program = oldprog; ProgCounter = oldpc }
    elif not (List.isEmpty fbody) then
      let state =
        evalProgram { state with Program = List.toArray fbody; ProgCounter = 0 }
      if state.IsReturning then state
      else incPC { state with Program = oldprog; ProgCounter = oldpc }
    else incPC state
  | BreakStmt (_) -> { state with ProgCounter = -1; IsBreaking = true }
  | NopStmt (_) -> incPC state
  | SolveStmt (e, annot) ->
    obtainVars state e annot Map.empty
    |> SMT.solve e state
    |> incPC
  | DebugStmt (e, fmt, annot) ->
    let state, v = evalExpr state e annot
    let str =
      match fmt with
      | OutHex -> toBV annot v |> BitVectorUtils.bvToBytes |> bytesToHexStr
      | OutAscii -> toBV annot v |> BitVectorUtils.bvToBytes |> bytesToStr
    printer.PrintLine (
      [ (NoColor, "[")
        (Red, "DBG")
        (NoColor, "] ")
        (Red, str) ])
    incPC state

and evalProgram state =
  let pc = state.ProgCounter
  if state.Program.Length <= pc || pc < 0 then state
  else evalProgram (evalStmt state state.Program.[pc])

let parseOverwriteRules macros =
  let rec loop acc = function
    | Overwrite (n, bv, soff, eoff) :: rest ->
      let rule = { NthWrite = n; SourceBV = bv; SOffset = soff; EOffset = eoff }
      loop (rule :: acc) rest
    | [] -> acc |> List.sortBy (fun r -> r.NthWrite)
  loop [] macros

let emptyState () =
  { Program = [||]
    ProgCounter = 0
    IsBreaking = false
    IsReturning = false
    FunctionMap = initFunctionMap ()
    VarMap = Map.empty
    RetStack = []
    FlagPath = None
    Connection = Network ("", 0)
    Stream = None
    StreamErrorSite = None
    Timeout = None
    Delay = 0
    WriteCount = 0
    WriteData = []
    SubmittedFlag = None
    OverwriteRules = []
    LibcPath = ""
    LibcHandle = None }

let initState conn poe flagPath libcPath delay =
  let overwrites = parseOverwriteRules poe.Macros
  { Program = [||]
    ProgCounter = 0
    IsBreaking = false
    IsReturning = false
    FunctionMap = initFunctionMap ()
    VarMap = Map.empty
    RetStack = []
    FlagPath = flagPath
    Connection = conn
    Stream = None
    StreamErrorSite = None
    Timeout = None
    Delay = delay
    WriteCount = 0
    WriteData = []
    SubmittedFlag = None
    OverwriteRules = overwrites
    LibcPath = libcPath
    LibcHandle = None }

let extractVarName annot = function
  | Var (name, _), _ -> name
  | _ -> err annot "Invalid argument expression."

let extractFunctionType (typeEnv: TypeEnvironment) name args annot =
  let exprs = List.map (extractVarName annot) args
  struct (exprs, typeEnv.FindSymbolType name None)

let rec initFunctions decls typeEnv state =
  match decls with
  | GlobalVarDecl (vType, vars, annot) :: rest ->
    let state' = evalVars state vType vars annot
    initFunctions rest typeEnv state'
  | FunDecl (name, args, body, annot) :: rest ->
    let struct (argExprs, ftyp) = extractFunctionType typeEnv name args annot
    let body = List.toArray body
    let state' = addFunction state name (RegularFunction (ftyp, argExprs, body))
    initFunctions rest typeEnv state'
  | ActDecl (name, args, body, annot) :: rest ->
    let struct (argExprs, ftyp) = extractFunctionType typeEnv name args annot
    let body = List.toArray body
    let state' = addFunction state name (ActionFunction (ftyp, argExprs, body))
    initFunctions rest typeEnv state'
  | SubmitDecl (stmts, _) :: rest ->
    let state' = { state with Program = List.toArray stmts; ProgCounter = 0 }
    initFunctions rest typeEnv state'
  | [] -> state

let printFlag = function
  | IntValue (n, _) ->
    printfn "dec   = {%d}" n
    printfn "hex   = {%x}" n
  | BitVecValue bv when bv.Count >= 8 ->
    BitVectorUtils.bvToBytes bv
    |> Array.map (sprintf "%02x")
    |> String.concat ""
    |> printfn "hex   = {%s}"
    BitVectorUtils.bvToBytes bv
    |> bytesToStr
    |> printfn "ascii = {%s}"
  | BitVecValue bv -> (* Less than a byte. *)
    [| for i in bv do yield i |]
    |> Array.map (fun bit -> if bit then "1" else "0")
    |> String.concat ""
    |> printfn "bin   = {%s}"
  | _ -> ()

let private runPoE conn poe typeEnv flagPath libcPath delay =
  let state =
    initState conn poe flagPath libcPath delay
    |> initFunctions poe.Decls typeEnv
    |> evalProgram
  if not state.IsReturning then err None "No return found in submit."
  let ret = popValue state None |> snd
  printFlag ret
  isEqual ret trueValue, List.rev state.WriteData, state.SubmittedFlag

let runPoEWithNetwork poe typeEnv ip port libcPath delay =
  let net = Network (ip, port)
  runPoE net poe typeEnv None libcPath delay

let runPoEWithPipe poe typeEnv flagPath prog libcPath args delay =
  let proc = Process (prog, args)
  runPoE proc poe typeEnv flagPath libcPath delay

let [<Literal>] private resultPath = "payload.dat"

let private writeToPayloadFile v =
  let bs =
    match v with
    | IntValue (n, t) ->
      BitVectorUtils.intToBV n (ExprType.toBitSize t)
      |> BitVectorUtils.bvToBytes
    | BitVecValue bv -> BitVectorUtils.bvToBytes bv
    | _ -> [||]
  IO.File.WriteAllBytes (resultPath, bs)

let runExpr expr =
  let state = emptyState ()
  let _, v = evalExpr state expr None
  printFlag v
  writeToPayloadFile v
