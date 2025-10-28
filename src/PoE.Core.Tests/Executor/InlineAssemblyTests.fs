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
type InlineAssemblyTests() =
  let [<Literal>] PoE1 = """
act func():
  bv code = x86 {{
    ; Simple move operations.
    mov al, 0 ; Set al to 0.
    mov bl, 0 ; Set bl to 0.
    mov cl, 0 ; Set cl to 0.
  }}
  return code

submit:
  return func()
"""

  let [<Literal>] PoE2 = """
act func():
  bv code = x86 {{
    je LABEL_1
    je LABEL_2
    jmp LABEL_3
    LABEL_1:
    inc al
    LABEL_2:
    inc bl
    LABEL_3:
    ret
  }}
  return code

submit:
  return func()
"""

  [<TestMethod>]
  member __.``Inline Assembly Test 1`` () =
    let ret, _, _ = runPoEWithEchoer PoE1
    let bv = getBitVecValue ret
    let bvBytes = BitVectorUtils.bvToBytes bv
    let expectedBytes = [| 0xB0uy; 0x00uy; 0xB3uy; 0x00uy; 0xB1uy; 0x00uy |]
    assertAreByteArrayEqual expectedBytes bvBytes

  [<TestMethod>]
  member __.``Inline Assembly Test 2 (Label)`` () =
    let ret, _, _ = runPoEWithEchoer PoE2
    let bv = getBitVecValue ret
    let bvBytes = BitVectorUtils.bvToBytes bv
    let expectedBytes = [| 0x74uy; 0x04uy; 0x74uy; 0x04uy; 0xEBuy; 0x04uy;
                           0xFEuy; 0xC0uy; 0xFEuy; 0xC3uy; 0xC3uy |]
    assertAreByteArrayEqual expectedBytes bvBytes
