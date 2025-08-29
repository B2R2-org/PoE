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

open B2R2.FrontEnd

type ConnectionInfo =
  | Network of ip: string * port: int
  | Process of prog: string * args: string

type EvalState = {
  Program: Stmt []
  ProgCounter: int
  IsBreaking: bool
  IsReturning: bool
  FunctionMap: Map<string, Function>
  VarMap: Map<string, EvalValue>
  RetStack: EvalValue list
  FlagPath: string option
  Connection: ConnectionInfo
  Stream: IStreamManager option
  StreamErrorSite: Annotation option
  Timeout: int option
  Delay: int
  WriteCount: int
  WriteData: byte [] list
  SubmittedFlag: string option
  OverwriteRules: OverwriteRule list
  LibcPath: string
  LibcHandle: BinHandle option
}

and Function =
  | RegularFunction of ExprType * args: string list * body: Stmt []
  | ActionFunction of ExprType * args: string list * body: Stmt []
  | BuiltinFunction of ExprType * BuiltInFunc

and EvalValue =
  | BitVecValue of BitVector
  | IntValue of int64 * ExprType
  | UnknownValue of ExprType

and OverwriteRule = {
  NthWrite: int
  SourceBV: BitVector
  SOffset: int
  EOffset: int
}

/// Built-in function base class.
and [<AbstractClass>] BuiltInFunc () =
  abstract member Name: string
  abstract member ReturnType: ExprType
  abstract member ParamTypes: ExprType list
  abstract member Execute: EvalState
                        -> EvalValue list
                        -> Annotation
                        -> EvalState * EvalValue

module EvalUtils =

  let err annot msg = Exception.err RuntimeException annot msg

  let getStream state annot =
    match state.Stream with
    | None -> err annot "Invalid access to stream."
    | Some s -> s

  let createStream state =
    match state.Connection with
    | Process (prog, args) ->
      new ProcessManager (prog, args) :> IStreamManager
    | Network (ip, port) -> new NetworkManager (ip, port) :> IStreamManager

  let addFunction state name fnvalue =
    { state with FunctionMap = Map.add name fnvalue state.FunctionMap }

  let findFunction state name annot =
    match Map.tryFind name state.FunctionMap with
    | None -> err annot ("Failed to find function " + name)
    | Some v -> v

  let addVar state name value =
    { state with VarMap = Map.add name value state.VarMap }

  let findVar state name annot =
    match Map.tryFind name state.VarMap with
    | None -> err annot ("Failed to find variable " + name)
    | Some v -> v

  let isAct state = state.Stream |> Option.isSome

  let adjustIntByType (n: int64) t annot =
    match t with
    | TypeInt8 | TypeUInt8 -> n &&& 0xffL
    | TypeInt16 | TypeUInt16 -> n &&& 0xffffL
    | TypeInt32 | TypeUInt32 -> n &&& 0xffffffffL
    | TypeInt64 | TypeUInt64 | TypeAnyInt -> n
    | _ -> err annot "Invalid integer input."

  /// Create an int value from a bitvector. If the bitvector is larger than the
  /// target type size, then take a sub-bitvector and forcefully cast it into a
  /// number.
  let intValue t (v: BitVector) isSigned =
    let len = ExprType.toBitSize t
    let subLen = min v.Count len
    let v =
      if isSigned then
        BitVectorUtils.subBV v 0 subLen |> BitVectorUtils.bvToInt
      else
        BitVectorUtils.subBV v 0 subLen |> BitVectorUtils.bvToUInt |> int64
    IntValue (adjustIntByType v t None, t)

  let bvValue t v =
    let bv = BitVectorUtils.intToBV v (ExprType.toBitSize t)
    BitVecValue (bv)

  let getEvalType = function
    | BitVecValue (bv) -> TypeBV (bv.Count)
    | IntValue (_, t) -> t
    | UnknownValue _ -> TypeUnit

  let toASTInt annot = function
    | IntValue (n, t) -> Int (n, t, annot)
    | _ -> err annot "Invalid AST conversion."

