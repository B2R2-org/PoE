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

namespace PoE.Core.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open System.Collections
open PoE
open PoE.Executor
open PoE.BitVectorUtils

module Samples =
  let poe1 = """
fun foo ():
    i8 a // unknown var
    i8 i = 0x10 // integer
    i8 j = (i + i) * le2be(0x1:i8)
    i8 k = j[4:7]
    dbghex i
    dbghex j
    dbgascii k
    solve (i * j = 42:i8 + a) // we will get a solution for 'a'
    return a

act bar ():
    i16 i = 0x10
    bv v = read(100)[0:15]
    write(i + i16 v)
    delay(10)
    return foo()

submit:
    return bar()
"""

  let poe2 = """
fun bar ():
  bv i = read(100)
  return i

fun foo ():
  bv a = bar()[0:9]
  bv b = bar()[-10:-1]
  return i16 a + i16 b

act a():
  return foo()

submit:
  return a()
"""

  let poe3 = """
fun f(i):
  write(i[0:7])
  return ()

fun g(i):
  return i + 60

act a():
  f(4141hs)
  f(4242hs)
  write(g(7)[0:7])
  f(4444hs)
  return ()

submit:
  return a()
"""

[<TestClass>]
type PoETestClass () =

  let testTranslation poeStr =
    let poe1, _ = Decoder.loadPoEFromString poeStr
    let out1 = poe1 |> Encoder.toString
    let poe2, _ = Decoder.loadPoEFromString out1
    let out2 = poe2 |> Encoder.toString
    Assert.AreEqual (out1, out2)

  [<TestMethod>]
  member __.``Serialization/Deserializion Test`` () =
    testTranslation Samples.poe1
    testTranslation Samples.poe2
    testTranslation Samples.poe3

[<TestClass>]
type BitVectorTestClass () =
  [<TestMethod>]
  member __.``Basic Conversion Test 1`` () =
    let bv = intToBV 42L 32
    let n = bvToInt bv
    Assert.AreEqual (32, bv.Length)
    Assert.AreEqual (42L, n)

  [<TestMethod>]
  member __.``Basic Conversion Test 2`` () =
    let bv = bigintToBV 42I
    let n = bvToBigInt bv
    Assert.AreEqual (42I, n)

  [<TestMethod>]
  member __.``Basic Conversion Test 3`` () =
    let bv = intToBV 42L 64
    let n = bvToInt bv
    Assert.AreEqual (64, bv.Length)
    Assert.AreEqual (42L, n)

  [<TestMethod>]
  member __.``Concatenation Test`` () =
    let bs1 = BitArray ([| 0x11uy; 0x22uy; 0x33uy; 0x44uy |])
    let bs2 = BitArray ([| 0x55uy; 0x66uy; 0x77uy; 0x88uy |])
    let res = concatBV bs1 bs2
    let expectation =
      BitArray ([| 0x11uy; 0x22uy; 0x33uy; 0x44uy
                   0x55uy; 0x66uy; 0x77uy; 0x88uy |])
    Assert.AreEqual (true, isBVEQ expectation res)

[<TestClass>]
type EvaluationTestClass () =
  [<TestMethod>]
  member __.``Evaluation Test (simple arithmetic 1)`` () =
    let v1 = IntValue (-1L &&& 0xffffffffL, TypeInt32)
    let v2 = IntValue (1L &&& 0xffffffffL, TypeInt32)
    let expectation = IntValue (0L, TypeInt32)
    let res = add v1 v2 None
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (simple arithmetic 2)`` () =
    let v1 = IntValue (-4L &&& 0xffffffffL, TypeInt32)
    let v2 = IntValue (2L, TypeInt32)
    let expectation = IntValue (2L, TypeInt32)
    let res = neg (sdiv v1 v2 None) None
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (simple arithmetic 3)`` () =
    let v1 = IntValue (0xff000000L, TypeInt32)
    let v2 = IntValue (8L, TypeInt32)
    let expectation =
      IntValue ([| 0uy; 0uy; 0xffuy; 0uy |] |> bytesToBV |> bvToInt, TypeInt32)
    let res = shr v1 v2 None
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (simple arithmetic 4)`` () =
    let v1 = IntValue (0xff000000L, TypeInt32)
    let v2 = IntValue (8L, TypeInt32)
    let expectation = IntValue (0xffff0000L, TypeInt32)
    let res = ashr v1 v2 None
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (conversion 1)`` () =
    let bv = BitVector (intToBV (0xffeeddccL) 32, None)
    let state = emptyState ()
    let soff = Int (24L, TypeInt64, None)
    let eoff = Int (31L, TypeInt64, None)
    let _, res = evalExtract state bv soff eoff None
    let expectation = BitVecValue (BitArray ([| 0xffuy |]))
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (conversion 2)`` () =
    let bv = BitVector (intToBV (0xffeeddccL) 32, None)
    let state = emptyState ()
    let soff = Int (0L, TypeInt64, None)
    let eoff = Int (7L, TypeInt64, None)
    let _, res = evalExtract state bv soff eoff None
    let expectation = BitVecValue (BitArray ([| 0xccuy |]))
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (conversion 3)`` () =
    let bv = BitVector (BitArray ([| 0xffuy |]), None)
    let state = emptyState ()
    let _, res = evalTypeCast state TypeInt16 bv None
    let expectation = IntValue (0xffffL, TypeInt16)
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (conversion 4)`` () =
    let bv = BitVector (BitArray ([| 0xffuy |]), None)
    let state = emptyState ()
    let _, res = evalTypeCast state TypeUInt16 bv None
    let expectation = IntValue (0xffL, TypeUInt16)
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (switch endian)`` () =
    let v = IntValue (0x11223344L, TypeInt32)
    let expectation = IntValue (0x44332211L, TypeInt32)
    let fn = Le2be ()
    let state = emptyState ()
    let _, res = fn.Execute state [ v ] None
    Assert.AreEqual (true, isEqual expectation res)

  [<TestMethod>]
  member __.``Evaluation Test (atoi)`` () =
    let bs = BitVecValue (BitArray ([| 0x33uy; 0x34uy; 0x35uy; 0x36uy |]))
    let expectation = IntValue (0xd80L, TypeInt16)
    let fn = Atoi ()
    let state = emptyState ()
    let _, res = fn.Execute state [ bs; IntValue (10L, TypeInt32) ] None
    Assert.AreEqual (res, expectation)

  [<TestMethod>]
  member __.``Evaluation Test (substr)`` () =
    let bs = BitVecValue (BitArray ([| 0x11uy; 0x22uy; 0x33uy; 0x44uy |]))
    let expectation = BitVecValue (BitArray ([| 0x22uy |]))
    let res = extract 8 15 bs None
    Assert.AreEqual (true, isEqual expectation res)

[<TestClass>]
type FactoryTestClass () =
  [<TestMethod>]
  member __.``Test genPoE`` () =
    Factory.genSimpleLeakPoE "" Unexploitable [||] |> ignore

[<TestClass>]
type ExecutorTestClass () =
  [<TestMethod>]
  member __.``Test runPoE`` () =
    let poe, typeEnv = Decoder.loadPoEFromString Samples.poe3
    let cwd = System.IO.Directory.GetCurrentDirectory ()
    let echoer = System.IO.Path.Combine (cwd, "Echoer")
    let _, written, _ = runPoEWithPipe poe typeEnv None echoer "" "" 0 None
    let expectation = [ [|65uy|]; [|66uy|]; [|67uy|]; [|68uy|]; ]
    let res = List.forall2 (fun a b -> a = b) expectation written
    Assert.AreEqual (true, res)
