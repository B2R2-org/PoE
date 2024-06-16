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
open System.Net.Sockets
open PoE.Stream

type NetworkManager (ip, port) =
  let defaultTimeout = 5000 (* 5 sec. *)
  let conn = new TcpClient (ip, port)
  let sock = conn.GetStream()
  let setTimeout = function
    | Some timeout ->
      sock.ReadTimeout <- timeout
      sock.WriteTimeout <- timeout
    | None ->
      sock.ReadTimeout <- defaultTimeout
      sock.WriteTimeout <- defaultTimeout
  interface IStreamManager with
    member __.Close () = conn.Close ()
    member __.Read n timeout =
      setTimeout timeout; read sock n
    member __.ReadErr n timeout =
      setTimeout timeout; [||] |> Some
    member __.ReadUntil str timeout =
      setTimeout timeout; readUntil sock str
    member __.ReadAll timeout =
      setTimeout timeout; readAll sock
    member __.Write delay data =
      setTimeout None; write sock delay data
  interface IDisposable with
    member __.Dispose () = conn.Close ()
