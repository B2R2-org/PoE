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

module PoE.Core.Tests.TestHelper

open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections
open PoE
open PoE.Executor
open PoE.BitVectorUtils

let [<Literal>] EchoerName = "Echoer"

let runPoEWithEchoer poe =
  let poe, typeEnv = Decoder.loadPoEFromString poe
  let cwd = System.IO.Directory.GetCurrentDirectory ()
  let echoerPath = System.IO.Path.Combine (cwd, EchoerName)
  runPoEWithPipe poe typeEnv None echoerPath "" "" 0 None

let assertAreByteArrayEqual (expected: byte[]) (actual: byte[]) =
  Assert.AreEqual (expected.Length, actual.Length,
                   "Byte arrays have different lengths.")
  for i in 0 .. expected.Length - 1 do Assert.AreEqual (expected[i], actual[i])
