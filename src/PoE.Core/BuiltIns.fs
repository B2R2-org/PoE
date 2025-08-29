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
open System.IO
open B2R2.FrontEnd
open B2R2.FrontEnd.BinFile
open PoE.StringUtils
open PoE.EvalUtils
open PoE.BitVectorUtils

[<AutoOpen>]
module private BuiltInHelper =
  let getLibcHandle state annot =
    match state.LibcHandle with
    | Some hdl -> state, hdl
    | None ->
      if String.IsNullOrEmpty state.LibcPath then err annot "Libc is not given."
      else
        let hdl = BinHandle(state.LibcPath)
        { state with LibcHandle = Some hdl }, hdl

  let toBV annot = function
    | IntValue (n, t) -> intToBV n (ExprType.toBitSize t)
    | BitVecValue bv -> bv
    | _ -> err annot "Invalid value used for bitvector operation."

  let prepareLibcFunc state args annot =
    let arg = List.head args
    let state, hdl = getLibcHandle state annot
    let name = arg |> toBV annot |> bvToBytes |> bytesToStr
    struct (state, name, hdl)

  let shouldOverwrite state =
    match state.OverwriteRules with
    | [] -> false
    | rule :: _ -> rule.NthWrite = state.WriteCount + 1

  let overwrite state bv =
    let rule = List.head state.OverwriteRules
    let cnt = rule.EOffset - rule.SOffset + 1
    replaceBV bv rule.SOffset cnt rule.SourceBV

type Read () =
  inherit BuiltInFunc ()
  override __.Name with get() = "read"
  override __.ReturnType with get() = TypeAnyBV
  override __.ParamTypes with get() = [ TypeAny ]
  override __.Execute state args annot =
    let returnBV maybeBuf =
      let v = Option.defaultValue [||] maybeBuf |> bytesToBV |> BitVecValue
      let isError = Option.isNone maybeBuf
      if isError then { state with StreamErrorSite = Some annot }, v
      else state, v
    match List.head args with
    | IntValue (v, _) ->
      let stream = getStream state annot
      if v < 0L then stream.ReadAll state.Timeout
      else stream.Read (Convert.ToInt32 v) state.Timeout
      |> returnBV
    | BitVecValue (delim) ->
      let stream = getStream state annot
      let delim = bvToBytes delim
      stream.ReadUntil delim state.Timeout
      |> returnBV
    | _ -> err annot "Invalid read expression."

type Write () =
  inherit BuiltInFunc ()
  override __.Name with get() = "write"
  override __.ReturnType with get() = TypeUnit
  override __.ParamTypes with get() = [ TypeAny ]
  override __.Execute state args annot =
    let v = List.head args
    let stream = getStream state annot
    let bv = toBV annot v
    let bv = if shouldOverwrite state then overwrite state bv else bv
    let data = bvToBytes bv
    let data =
      if stream.Write state.Delay data then data :: state.WriteData
      else state.WriteData
    { state with WriteData = data; WriteCount = state.WriteCount + 1 },
    UnknownValue TypeUnit

type Atoi () =
  inherit BuiltInFunc ()
  override __.Name with get() = "atoi"
  override __.ReturnType with get() = TypeAnyInt
  override __.ParamTypes with get() = [ TypeAnyBV; TypeAnyInt ]
  override __.Execute state args annot =
    match args with
    | BitVecValue bv :: IntValue (basenum, _) :: _ ->
      let s = bvToBytes bv |> bytesToStr
      let n = Convert.ToInt64 (s.Trim(), int basenum)
      state, IntValue (n, ExprType.ofInt64 n)
    | BitVecValue bv :: [] ->
      let s = bvToBytes bv |> bytesToStr
      let n = Convert.ToInt64 (s.Trim(), 10)
      state, IntValue (n, ExprType.ofInt64 n)
    | _ -> err annot "Invalid parameter for atoi."

type Itoa () =
  inherit BuiltInFunc ()
  override __.Name with get() = "itoa"
  override __.ReturnType with get() = TypeAnyBV
  override __.ParamTypes with get() = [ TypeAnyInt; TypeAnyInt ]
  override __.Execute state args annot =
    match args with
    | IntValue (v, _) :: IntValue (10L, _) :: _ ->
      state, v.ToString () |> strToBV |> BitVecValue
    | IntValue (v, _) :: IntValue (16L, _) :: _ ->
      state, v.ToString ("X") |> strToBV |> BitVecValue
    | IntValue (v, _) :: [] ->
      state, v.ToString () |> strToBV |> BitVecValue
    | _ -> err annot "Invalid parameter for atoi."

type Delay () =
  inherit BuiltInFunc ()
  override __.Name with get() = "delay"
  override __.ReturnType with get() = TypeUnit
  override __.ParamTypes with get() = [ TypeAnyInt ]
  override __.Execute state args annot =
    match args with
    | IntValue (v, _) :: _ ->
      Threading.Thread.Sleep (Convert.ToInt32 v)
      state, UnknownValue TypeUnit
    | _ -> err annot "Invalid parameter for atoi."

type Rtrim () =
  inherit BuiltInFunc ()
  override __.Name with get() = "rtrim"
  override __.ReturnType with get() = TypeAnyBV
  override __.ParamTypes with get() = [ TypeAnyBV ]
  override __.Execute state args annot =
    match args with
    | BitVecValue bv :: _ ->
      state, trimEnd bv |> BitVecValue
    | _ -> err annot "Invalid parameter for atoi."

type Le2be () =
  inherit BuiltInFunc ()

  let swap16 n =
    ((n >>> 8) &&& 0x00000000000000ffL)
    ||| ((n <<< 8) &&& 0x000000000000ff00L)

  let swap32 n =
    ((n >>> 24) &&& 0x00000000000000ffL)
    ||| ((n >>> 8) &&& 0x000000000000ff00L)
    ||| ((n <<< 8) &&& 0x0000000000ff0000L)
    ||| ((n <<< 24) &&& 0x00000000ff000000L)

  let swap64 n =
    ((n >>> 56) &&& 0x00000000000000ffL)
    ||| ((n >>> 40) &&& 0x000000000000ff00L)
    ||| ((n >>> 24) &&& 0x0000000000ff0000L)
    ||| ((n >>> 8) &&& 0x00000000ff000000L)
    ||| ((n <<< 8) &&& 0x000000ff00000000L)
    ||| ((n <<< 24) &&& 0x0000ff0000000000L)
    ||| ((n <<< 40) &&& 0x00ff000000000000L)
    ||| ((n <<< 56) &&& 0xff00000000000000L)

  override __.Name with get() = "le2be"
  override __.ReturnType with get() = TypeAnyInt
  override __.ParamTypes with get() = [ TypeAnyInt ]
  override __.Execute state args annot =
    match args with
    | IntValue (v, TypeInt8) :: _ -> state, IntValue (v, TypeInt8)
    | IntValue (v, TypeUInt8) :: _ -> state, IntValue (v, TypeUInt8)
    | IntValue (v, TypeInt16) :: _ -> state, IntValue (swap16 v, TypeInt16)
    | IntValue (v, TypeUInt16) :: _ -> state, IntValue (swap16 v, TypeUInt16)
    | IntValue (v, TypeInt32) :: _ -> state, IntValue (swap32 v, TypeInt32)
    | IntValue (v, TypeUInt32) :: _ -> state, IntValue (swap32 v, TypeUInt32)
    | IntValue (v, TypeInt64) :: _ -> state, IntValue (swap64 v, TypeInt64)
    | IntValue (v, TypeUInt64) :: _ -> state, IntValue (swap64 v, TypeUInt64)
    | IntValue (v, TypeAnyInt) :: tl ->
      let t = ExprType.ofInt64 v
      __.Execute state (IntValue (v, t) :: tl) annot
    | _ -> err annot "Invalid parameter for le2be."

type Be2le () =
  inherit Le2be ()
  override __.Name with get() = "le2be"

/// Network text presentation (P) to numeric (N). Similar to inet_pton.
type P2n () =
  inherit BuiltInFunc ()
  override __.Name with get() = "p2n"
  override __.ReturnType with get() = TypeInt32
  override __.ParamTypes with get() = [ TypeAnyBV ]
  override __.Execute state args annot =
    match args with
    | BitVecValue ip :: _ ->
      let ipstr = bvToBytes ip |> bytesToStr
      match ipstr.Split ([| '.' |]) with
      | [| a; b; c; d |] ->
       let a = Convert.ToByte a |> uint32 |> int64
       let b = Convert.ToByte b |> uint32 |> int64
       let c = Convert.ToByte c |> uint32 |> int64
       let d = Convert.ToByte d |> uint32 |> int64
       let n = (d <<< 24) ||| (c <<< 16) ||| (b <<< 8) ||| a
       state, IntValue (n, TypeInt32)
      | _ -> err annot "Invalid ipv4 address."
    | _ -> err annot "Invalid parameter for p2n."

type Replace () =
  inherit BuiltInFunc ()
  override __.Name with get() = "replace"
  override __.ReturnType with get() = TypeAnyBV
  override __.ParamTypes with get() =
    [ TypeAnyBV; TypeAnyInt; TypeAnyInt; TypeAnyBV ]
  override __.Execute state args annot =
    match args with
    | BitVecValue dst :: IntValue (soff, _)
                      :: IntValue (eoff, _)
                      :: BitVecValue src
                      :: _ ->
      let soff, eoff = int soff, int eoff
      if soff >= 0 && eoff = -1 && dst.Length > soff then
        state,
        BitVecValue (replaceBV dst soff (dst.Length - soff) src)
      elif soff < 0 && eoff < 0 && soff < eoff then
        let soff = dst.Length + soff
        state,
        BitVecValue (replaceBV dst soff (soff + eoff + 1) src)
      else
        state,
        BitVecValue (replaceBV dst soff (eoff - soff + 1) src)
    | _ -> err annot "Invalid parameter for replace."

type BitLen () =
  inherit BuiltInFunc ()
  override __.Name with get() = "bitlen"
  override __.ReturnType with get() = TypeInt64
  override __.ParamTypes with get() = [ TypeAny ]
  override __.Execute state args annot =
    match args with
    | BitVecValue bv :: _ -> state, IntValue (int64 bv.Count, TypeInt64)
    | IntValue (_, t) :: _ ->
      state, IntValue (int64 (ExprType.toBitSize t), TypeInt64)
    | _ -> err annot "Invalid parameter for bitlen."

type ByteLen () =
  inherit BuiltInFunc ()
  override __.Name with get() = "bytelen"
  override __.ReturnType with get() = TypeInt64
  override __.ParamTypes with get() = [ TypeAny ]
  override __.Execute state args annot =
    match args with
    | BitVecValue bv :: _ -> state, IntValue (int64 bv.Count / 8L, TypeInt64)
    | IntValue (_, t) :: _ ->
      state, IntValue (int64 (ExprType.toSize t), TypeInt64)
    | _ -> err annot "Invalid parameter for bytelen."

type Dump () =
  inherit BuiltInFunc ()
  override __.Name with get() = "dump"
  override __.ReturnType with get() = TypeUnit
  override __.ParamTypes with get() = [ TypeAnyBV; TypeAnyBV ]
  override __.Execute state args annot =
    match args with
    | BitVecValue bv :: BitVecValue path :: _ ->
      let bs = bvToBytes bv
      let path = bvToBytes path |> bytesToStr
      File.WriteAllBytes (path, bs)
      state, UnknownValue TypeUnit
    | _ -> err annot "Invalid parameter for dump."

type LibcFuncAddr () =
  inherit BuiltInFunc ()
  override __.Name with get() = "libcFuncAddr"
  override __.ReturnType with get() = TypeInt64
  override __.ParamTypes with get() = [ TypeAnyBV ]
  override __.Execute state args annot =
    let struct (state, name, hdl) = prepareLibcFunc state args annot
    (hdl.File :?> ELFBinFile).Symbols.DynamicSymbols
    |> Array.filter (fun s -> s.SymName = name)
    |> Array.tryHead
    |> function
      | Some s -> state, IntValue (int64 s.Addr, TypeInt64)
      | _ -> err annot "Libc function not found."

type LibcStrAddr () =
  inherit BuiltInFunc ()
  override __.Name with get() = "libcStrAddr"
  override __.ReturnType with get() = TypeInt64
  override __.ParamTypes with get() = [ TypeAnyBV ]
  override __.Execute state args annot =
    let struct (state, name, hdl) = prepareLibcFunc state args annot
    let pattern = strToBytes (name + "\x00")
    (hdl.File :?> ELFBinFile).ProgramHeaders
    |> Array.filter (fun ph ->
      ELF.ProgramHeader.FlagsToPerm ph.PHFlags = Permission.Readable)
    |> Array.tryPick (fun ph ->
      hdl.ReadBytes(ph.PHAddr, int ph.PHFileSize)
      |> B2R2.ByteArray.findIdxs ph.PHAddr pattern
      |> List.tryHead)
    |> function
      | Some addr -> state, IntValue (int64 addr, TypeInt64)
      | _ -> err annot "Invalid libc string"

type Pause () =
  inherit BuiltInFunc ()
  override __.Name with get() = "pause"
  override __.ReturnType with get() = TypeUnit
  override __.ParamTypes with get() = []
  override __.Execute state args annot =
    Console.WriteLine ("[PAUSE until the enter key is pressed.]")
    Console.ReadLine () |> ignore
    state, UnknownValue TypeUnit

module BuiltIns =
  let functions =
    [| Read () :> BuiltInFunc
       Write () :> BuiltInFunc
       Atoi () :> BuiltInFunc
       Itoa () :> BuiltInFunc
       Delay () :> BuiltInFunc
       Rtrim () :> BuiltInFunc
       Le2be () :> BuiltInFunc
       Be2le () :> BuiltInFunc
       P2n () :> BuiltInFunc
       Replace () :> BuiltInFunc
       BitLen () :> BuiltInFunc
       ByteLen () :> BuiltInFunc
       Dump () :> BuiltInFunc
       LibcFuncAddr () :> BuiltInFunc
       LibcStrAddr () :> BuiltInFunc
       Pause () :> BuiltInFunc |]
