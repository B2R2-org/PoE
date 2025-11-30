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

namespace PoE.Core.Tests.Executor

open Microsoft.VisualStudio.TestTools.UnitTesting
open PoE
open PoE.Core.Tests.EvalValueHelper
open PoE.Core.Tests.TestHelper

[<TestClass>]
type WriteAfterTests() =
  let [<Literal>] PoE1 = """
act func():
  write("Input a password:\n")
  bv bytes = writeafter("password:", "kimchi\n")
  bv rest = read(-1)
  return bytes . rest

submit:
  return func()
"""

  let [<Literal>] PoE2 = """
act func():
  bv v = writeafter("password:", "kimchi")
  return v

submit:
  return func()
"""

  [<TestMethod>]
  member __.``WriteAfter Test 1`` () =
    let ret, _, _ = runPoEWithEchoer PoE1
    let bv = getBitVecValue ret
    let bvBytes = BitVectorUtils.bvToBytes bv
    let expectedString = "Input a password:\nkimchi\n"
    let expectedBytes = getByteArrayFromStringWithProperNewLines expectedString
    assertAreByteArrayEqual expectedBytes bvBytes

  [<TestMethod>]
  member __.``WriteAfter Test 2`` () =
    Assert.ThrowsException<RuntimeException>(fun () ->
      runPoEWithEchoer PoE2 |> ignore)
    |> ignore
