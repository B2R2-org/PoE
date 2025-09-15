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

module PoE.BitVectorUtils

open System
open System.Numerics
open System.Collections

let bytesToBV (bytes: byte []) = BitArray (bytes)

let strToBV str = str |> StringUtils.strToBytes |> bytesToBV

let hexstrToBV (str: string) =
  let str = str.Substring (0, String.length str - 2)
  let str = if String.length str % 2 = 1 then "0" + str else str
  let n = String.length str / 2
  let bs = Array.zeroCreate<byte> (n)
  for i = 0 to (n - 1) do
    bs.[i] <- Convert.ToByte(str.Substring(i * 2, 2), 16)
  BitArray (bs)

let binstrToBV (str: string) =
  str.Substring (0, String.length str - 2)
  |> Seq.toArray
  |> Array.map (function
    | '0' -> false
    | '1' -> true
    | _ -> raise (RuntimeException "Bad number for binstring."))
  |> BitArray

let bvToBytes (bv: BitVector) =
  let delta = if bv.Length % 8 > 0 then 1 else 0
  let arr: byte [] = Array.zeroCreate (bv.Length / 8 + delta)
  bv.CopyTo (arr, 0)
  arr

let bvToBools (bv: BitVector) =
  let arr: bool [] = Array.zeroCreate (bv.Length)
  bv.CopyTo (arr, 0)
  arr

let bvToBigInt (bv: BitVector) =
  BigInteger (bvToBytes bv)

let bvToInt (bv: BitVector) =
  let len = bv.Length
  if len <= 8 then
    let arr: byte [] = Array.zeroCreate (8)
    bv.CopyTo (arr, 0)
    arr.[0] |> int8 |> int64
  elif len <= 16 then
    let arr: byte [] = Array.zeroCreate (16)
    bv.CopyTo (arr, 0)
    BitConverter.ToInt16 (arr, 0) |> int64
  elif len <= 32 then
    let arr: byte [] = Array.zeroCreate (32)
    bv.CopyTo (arr, 0)
    BitConverter.ToInt32 (arr, 0) |> int64
  elif len <= 64 then
    let arr: byte [] = Array.zeroCreate (64)
    bv.CopyTo (arr, 0)
    BitConverter.ToInt64 (arr, 0)
  else raise (RuntimeException "Big number for an int64.")

let bvToUInt (bv: BitVector) =
  let len = bv.Length
  if len <= 8 then
    let arr: byte [] = Array.zeroCreate (8)
    bv.CopyTo (arr, 0)
    arr.[0] |> uint8 |> uint64
  elif len <= 16 then
    let arr: byte [] = Array.zeroCreate (16)
    bv.CopyTo (arr, 0)
    BitConverter.ToUInt16 (arr, 0) |> uint64
  elif len <= 32 then
    let arr: byte [] = Array.zeroCreate (32)
    bv.CopyTo (arr, 0)
    BitConverter.ToUInt32 (arr, 0) |> uint64
  elif len <= 64 then
    let arr: byte [] = Array.zeroCreate (64)
    bv.CopyTo (arr, 0)
    BitConverter.ToUInt64 (arr, 0)
  else raise (RuntimeException "Big number for an int64.")

let bigintToBV (n: bigint) =
  n.ToByteArray () |> bytesToBV

let intToBV (n: int64) (sz: int) =
  let bs = BitConverter.GetBytes (n)
  match sz with
  | 1 -> BitArray ([| if n = 1L then yield true else yield false |])
  | 8 -> Array.sub bs 0 1 |> bytesToBV
  | 16 -> Array.sub bs 0 2 |> bytesToBV
  | 32 -> Array.sub bs 0 4 |> bytesToBV
  | 64 -> Array.sub bs 0 8 |> bytesToBV
  | s -> raise (RuntimeException ("Invalid integer size " + s.ToString ()))

let subBV (bv: BitVector) low count =
  if (low < 0) || count < 0 || (low + count > bv.Length) then
    raise (RuntimeException "Invalid offsets for subBV.")
  else
    let arr = bvToBools bv
    BitArray (Array.sub arr low count)

let replaceBV (bv: BitVector) low count (src: BitVector) =
  if (low < 0) || count < 0 || (low + count > bv.Length) then
    raise (RuntimeException "Invalid offsets for subBV.")
  elif src.Length <> count then
    raise (RuntimeException "Invalid src given for replaceBV.")
  else
    let bvArr = bvToBools bv
    let srcArr = bvToBools src
    Array.blit srcArr 0 bvArr low count
    BitArray (bvArr)

let rec trimEnd (bv: BitVector) =
  if bv.Length = 0 then bv
  else
    let bs = bvToBytes bv
    let last = bs.[bs.Length - 1]
    if last = '\n'B || last = '\r'B || last = '\t'B || last = ' 'B then
      trimEnd (bytesToBV bs.[ .. bs.Length - 2])
    else bv

let isBVEQ (bv1: BitVector) (bv2: BitVector) =
  if bv1.Length <> bv2.Length then false
  else Array.forall2 (=) (bvToBools bv1) (bvToBools bv2)

let private extendBV (bv: BitVector) targetLen head =
  let bvLen = bv.Length
  if bvLen > targetLen then bv
  else
    let arr = bvToBools bv
    let h = if Option.isNone head then arr.[arr.Length - 1] else Option.get head
    BitArray (Array.concat [ arr; Array.create (targetLen - bvLen) h ])

let signExtBV (bv: BitVector) targetLen = extendBV bv targetLen None
let zeroExtBV (bv: BitVector) targetLen = extendBV bv targetLen (Some false)

let concatBV (bv1: BitVector) (bv2: BitVector): BitVector =
  let arr1 = bvToBools bv1
  let arr2 = bvToBools bv2
  BitArray (Array.concat [ arr1; arr2 ])

let multiplyBV (bv: BitVector) count =
  let rec loop count acc =
    if count <= 0 then raise (DecodingException "Cannot multiply by zero.")
    elif count = 1 then acc
    else loop (count - 1) (concatBV acc bv)
  loop count bv
