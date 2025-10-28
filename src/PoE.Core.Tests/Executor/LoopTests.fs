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
open PoE.Core.Tests.TestHelper
open PoE.Core.Tests.EvalValueHelper

[<TestClass>]
type LoopTests () =
  let [<Literal>] PoE1 = """
act func():
  u8 i = 0
  u8 j = 0
  u8 k = 0
  while true
    i := i + 1
    if i = 10 then
      break
    else
      j := j + 1
    k := k + 1
  return i . j . k

submit:
  return func()
"""

  let [<Literal>] PoE2 = """
act func():
  u8 i = 0
  u8 j = 0
  u8 k = 0
  while true
    i := i + 1
    if i = 10 then
      write(i . j . k)
      return 11
    else
      j := j + 1
    k := k + 1
  return i . j . k

submit:
  return func()
"""

  [<TestMethod>]
  member __.``Loop Test 1`` () =
    let ret, _, _ = runPoEWithEchoer PoE1
    let bv = getBitVecValue ret
    let bvBytes = BitVectorUtils.bvToBytes bv
    let expectedBytes = [| 10uy; 9uy; 9uy |]
    assertAreByteArrayEqual expectedBytes bvBytes

  [<TestMethod>]
  member __.``Loop Test 2`` () =
    let ret, written, _ = runPoEWithEchoer PoE2
    let expectedRet = IntValue (11L, TypeAnyInt)
    let expectedWritten = [
      [| 10uy; 9uy; 9uy |]
    ]
    Assert.AreEqual (expectedRet, ret)
    Assert.AreEqual (expectedWritten, written)
