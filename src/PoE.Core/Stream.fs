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

module PoE.Stream

open System
open System.IO
open System.Threading.Tasks
open B2R2.RearEnd.Utils

let [<Literal>] DefaultBufSize = 4096
let [<Literal>] DefaultTimeout = 5000 (* 5 sec. *)
let [<Literal>] InfiniteTimeout = -1

/// A flag that controls whether the replayer will print out read/write
/// messages.
let mutable debugStream = false

let printColor (str: string) color =
  Terminal.Out.PrintLine(ColoredString(color, str.TrimEnd()))
  Terminal.Out.Flush ()

let printDbg isOut bs =
  if debugStream then
    if isOut then printColor (StringUtils.bytesToStr bs) DarkYellow
    else printColor (StringUtils.bytesToStr bs) Blue
  else ()
  bs

let private readFromStream (s: Stream) n timeout =
  let buf = Array.zeroCreate n
  let rec read buf idx n =
    let size = s.Read (buf, idx, n)
    if size = 0 then None
    elif size = n then Some buf
    else read buf (idx + size) (n - size)
  let task = Task.Run (fun () -> read buf 0 n)
  let timeout = Option.defaultValue DefaultTimeout timeout
  if task.Wait (timeout) then task.Result
  else raise (RuntimeException "Read timeout.")

let read stream n timeout =
  match readFromStream stream n timeout with
  | Some buf -> buf |> printDbg true |> Some
  | None -> None

let private endWith (data : 'a array) (target : 'a array) =
  let tLen = target.Length
  let dLen = data.Length
  if dLen < tLen  then false
  else (Array.sub data (dLen-tLen) tLen) = target

let private readUntilFromStream stream target timeout =
  let rec readUntil cur =
    if endWith cur target then Some cur
    else
      match readFromStream stream 1 (Some InfiniteTimeout) with
      | Some buf -> buf |> Array.append cur |> readUntil
      | None -> None
  let cur = Array.zeroCreate 0
  let task = Task.Run (fun () -> readUntil cur)
  let timeout = Option.defaultValue DefaultTimeout timeout
  if task.Wait (timeout) then task.Result
  else raise (RuntimeException "ReadUntil timeout.")

let readUntil stream target timeout =
  match readUntilFromStream stream target timeout with
  | Some buf -> buf |> printDbg true |> Some
  | None -> None

let rec private readAllFromStream (s : Stream) timeout =
  let mutable buf = [||]
  let rec readAll () =
    let tmp = Array.zeroCreate DefaultBufSize
    let size = s.Read(tmp, 0, DefaultBufSize)
    if size < DefaultBufSize then
      buf <- Array.sub tmp 0 size |> Array.append buf
    else buf <- Array.append buf tmp; readAll ()
  let task = Task.Run (fun () -> readAll ())
  let timeout = Option.defaultValue DefaultTimeout timeout
  if task.Wait (timeout) then buf
  else raise (RuntimeException "ReadAll timeout.")

let readAll stream timeout =
  readAllFromStream stream timeout |> printDbg true |> Some

let private writeToStream (s : Stream) data =
  try s.Write(data, 0, data.Length); s.Flush(); true
  with _ -> false

let write stream delay data =
  let success = data |> writeToStream stream
  data |> printDbg false |> ignore
  if delay > 0 then Threading.Thread.Sleep (delay) else ()
  success
