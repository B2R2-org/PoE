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
open System.Diagnostics
open System.Collections.Specialized
open PoE.Stream

[<AutoOpen>]
module private ProcHelper =
  let kill pid =
    Process.GetProcesses ()
    |> Array.iter (fun p -> if p.Id = pid then p.Kill () else ())

type ProcessManager (binPath, args) =
  let pInfo =
    ProcessStartInfo (
      RedirectStandardInput = true,
      RedirectStandardOutput = true,
      RedirectStandardError = true,
      UseShellExecute = false,
      FileName = binPath,
      Arguments = args )
  let proc = new Process (StartInfo = StdBuf.setPreloadPath pInfo)
  do proc.Start () |> ignore
  let inStrm = proc.StandardInput.BaseStream
  let outStrm = proc.StandardOutput.BaseStream
  let errStrm = proc.StandardError.BaseStream

  interface IStreamManager with
    member __.Pid = Some proc.Id
    member __.OnCreate () = ()
    member __.OnClose () = kill proc.Id
    member __.Read n timeout = read outStrm n timeout
    member __.ReadErr n timeout = read errStrm n timeout
    member __.ReadUntil str timeout = readUntil outStrm str timeout
    member __.ReadAll timeout = readAll outStrm timeout
    member __.Write delay data = write inStrm delay data

  interface IDisposable with
    member __.Dispose () =
      try proc.Kill () with _ -> ()
