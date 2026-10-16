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
open PoE
open PoE.BitVectorUtils
open PoE.Core.Tests.EvalValueHelper
open PoE.Core.Tests.TestHelper

[<TestClass>]
type RegressionTests () =
  // Failed decoding can leave the shared lexer context inside a block.
  let resetLexer () =
    Lexer.emptyContext.Offset <- 0
    Lexer.emptyContext.Line <- 1
    Lexer.emptyContext.Stack.Clear ()
    Lexer.emptyContext.Stack.Push 0
  let decode source =
    resetLexer ()
    Decoder.loadPoEFromString source
  let run source =
    resetLexer ()
    runPoEWithEchoer source |> fun (ret, _, _) -> ret
  let eval expr = run ($"submit:\n  return {expr}\n")
  let encode source = decode source |> fst |> Encoder.toString
  let evalUnchecked expr =
    resetLexer ()
    Executor.evalExpr (Executor.emptyState ())
      (Decoder.loadExprFromString expr) None |> snd

  // #33: signed atoi inference, including the separate explicit-base path.
  [<DataTestMethod>]
  [<DataRow("atoi(\"127\")", 127L, "i8")>]
  [<DataRow("atoi(\"128\")", 128L, "i16")>]
  [<DataRow("atoi(\"32767\")", 32767L, "i16")>]
  [<DataRow("atoi(\"32768\")", 32768L, "i32")>]
  [<DataRow("atoi(\"2147483647\")", 2147483647L, "i32")>]
  [<DataRow("atoi(\"2147483648\")", 2147483648L, "i64")>]
  [<DataRow("atoi(\"-1\")", -1L, "i64")>]
  [<DataRow("atoi(\"80\", 16)", 128L, "i16")>]
  member __.``atoi preserves values at signed width boundaries``
    (expr: string, expected: int64, typ: string) =
    match eval expr with
    | IntValue (n, t) ->
      Assert.AreEqual (expected, n)
      Assert.AreEqual (typ, ExprType.toString t)
    | _ -> Assert.Fail "Expected an integer."
    Assert.AreEqual (IntValue (expected, TypeInt64), eval ($"i64 ({expr})"))

  [<DataTestMethod>]
  [<DataRow("atoi(\"255\") + 1 = 256")>]
  [<DataRow("itoa(atoi(\"-2\") + 1) = \"-1\"")>]
  [<DataRow("atoi(\"-1\", 10) + 0 = atoi(\"-1\", 10)")>]
  member __.``atoi arithmetic agrees with equality and formatting`` (expr: string) =
    Assert.IsTrue (Executor.isTrue (eval expr), expr)

  // #35 and the endian regression encountered while fixing #33.
  [<DataTestMethod>]
  [<DataRow("0x1122:u16", 0x2211L, "u16")>]
  [<DataRow("0x80", 128L, "i8")>]
  [<DataRow("0x8000", 128L, "i16")>]
  [<DataRow("0x80000000", 128L, "i32")>]
  member __.``both endian builtins preserve explicit and inferred widths``
    (literal: string, expected: int64, typ: string) =
    for name in ["le2be"; "be2le"] do
      match eval ($"{name}({literal})") with
      | IntValue (n, t) ->
        Assert.AreEqual (expected, n, name)
        Assert.AreEqual (typ, ExprType.toString t, name)
      | _ -> Assert.Fail "Expected an integer."

  // #34: suffix, interior, single-bit, and unchanged positive ranges.
  [<DataTestMethod>]
  [<DataRow(-8, -1, "aahs", "001122aahs")>]
  [<DataRow(-16, -9, "aahs", "0011aa33hs")>]
  [<DataRow(-1, -1, "true", "001122b3hs")>]
  [<DataRow(24, 31, "aahs", "001122aahs")>]
  member __.``replacement offsets are inclusive``
    (low: int, high: int, replacement: string, expected: string) =
    let source =
      $"submit:\n  bv v = 00112233hs\n  v[{low}:{high}] := {replacement}\n  return v\n"
    Assert.IsTrue (isBVEQ (hexstrToBV expected) (run source |> getBitVecValue))

  // #36: each row exercises a distinct bound conversion or termination case.
  [<DataTestMethod>]
  [<DataRow("-2", "1", 4L)>]
  [<DataRow("0", "-1", 0L)>]
  [<DataRow("-3", "-1", 3L)>]
  [<DataRow("i8 254", "1:i8", 4L)>]
  [<DataRow("atoi(\"0\")", "atoi(\"300\")", 301L)>]
  [<DataRow("atoi(\"-2\")", "atoi(\"1\")", 4L)>]
  [<DataRow("254:u8", "255:u8", 2L)>]
  [<DataRow("-9223372036854775807 - 1", "-9223372036854775807", 2L)>]
  [<DataRow("0x7fffffffffffffff:u64", "0x8000000000000000:u64", 2L)>]
  [<DataRow("0xfffffffffffffffe:u64", "0xffffffffffffffff:u64", 2L)>]
  [<DataRow("-1", "1:u64", 3L)>]
  [<DataRow("fehs", "ffhs", 2L)>]
  member __.``for loops count inclusive ranges without wrapping``
    (first: string, last: string, expected: int64) =
    // Stop a faulty loop after one extra iteration instead of hanging the suite.
    let source = $"""
submit:
  i64 count = 0
  for i = {first} to {last}
    count := count + 1
    if count > {expected} then
      break
    else
      ()
  return count
"""
    Assert.AreEqual (IntValue (expected, TypeInt64), run source, source)

  [<TestMethod>]
  member __.``positive loops retain unsigned counter compatibility`` () =
    let source =
      "submit:\n  u64 count = 0\n  for i = 0 to 2\n    count := count + (i + 1:u64)\n  return count\n"
    Assert.AreEqual (IntValue (6L, TypeUInt64), run source)

  [<TestMethod>]
  member __.``break exits only the innermost for loop`` () =
    let source = """
submit:
  i64 count = 0
  for i = -1 to 1
    for j = -2 to 2
      count := count + 1
      break
    count := count + i
  return count
"""
    Assert.AreEqual (IntValue (3L, TypeInt64), run source)

  [<DataTestMethod>]
  [<DataRow("break", 9L)>]
  [<DataRow("return 7", 7L)>]
  member __.``for loops honor break and return`` (exit: string, expected: int64) =
    let source = $"submit:\n  for i = -2 to 1\n    {exit}\n  return 9\n"
    Assert.AreEqual (IntValue (expected, TypeAnyInt), run source)

  [<TestMethod>]
  member __.``loop bounds must fit a common 64-bit range`` () =
    let source =
      "submit:\n  for i = -1 to 0xffffffffffffffff:u64\n    break\n  return ()\n"
    let exn = Assert.ThrowsException<RuntimeException>(fun () -> run source |> ignore)
    StringAssert.Contains (exn.Message, "64-bit")

  // #41 and #42: compare decoded values as well as stable printed output.
  [<DataTestMethod>]
  [<DataRow("10bs")>]
  [<DataRow("010110010bs")>]
  member __.``binary pretty printing preserves bit order and length``
    (literal: string) =
    let source = $"submit:\n  return {literal}\n"
    let output = encode source
    Assert.IsTrue (isBVEQ (run source |> getBitVecValue)
                         (run output |> getBitVecValue))
    Assert.AreEqual (output, encode output)

  [<TestMethod>]
  member __.``multiplied strings round trip every byte`` () =
    let escaped = [| 0 .. 255 |] |> Array.map (sprintf "\\x%02x") |> String.concat ""
    let output = encode ($"submit:\n  return \"{escaped}\"x2\n")
    let expected = Array.append [| 0uy .. 255uy |] [| 0uy .. 255uy |]
    assertAreByteArrayEqual expected (run output |> getBitVecValue |> bvToBytes)
    Assert.AreEqual (output, encode output)

  [<TestMethod>]
  member __.``pretty printing retains both debug statement forms`` () =
    let output = encode "submit:\n  dbghex 41hs\n  dbgascii 42hs\n  return ()\n"
    StringAssert.Contains (output, "dbghex 41hs")
    StringAssert.Contains (output, "dbgascii 42hs")
    Assert.AreEqual (output, encode output)

  // #43: reject expired loop bindings while allowing the name to be reused.
  [<TestMethod>]
  member __.``loop variables are unavailable after the loop`` () =
    let exn = Assert.ThrowsException<DecodingException>(fun () ->
      decode "submit:\n  for i = 0 to 1\n    ()\n  return i\n" |> ignore)
    StringAssert.Contains (exn.Message, "Unknown symbol `i'")

  [<TestMethod>]
  member __.``loop names can be reused after their scope ends`` () =
    let source = """
submit:
  i64 count = 0
  for i = 0 to 1
    count := count + i64 i
  for i = 0 to 2
    count := count + i64 i
  i64 i = 9
  return count + i
"""
    Assert.AreEqual (IntValue (13L, TypeInt64), run source)

  // #44: expression/statement checks and runtime dispatch are separate entry points.
  [<DataTestMethod>]
  [<DataRow("fun", "f()")>]
  [<DataRow("fun", "f(1, 2)")>]
  [<DataRow("act", "f()")>]
  [<DataRow("act", "f(1, 2)")>]
  member __.``user calls reject missing and extra arguments``
    (kind: string, call: string) =
    for body in [$"return {call}"; $"{call}\n  return ()"] do
      let source = $"{kind} f(x):\n  return x\nsubmit:\n  {body}\n"
      let exn = Assert.ThrowsException<DecodingException>(fun () -> decode source |> ignore)
      StringAssert.Contains (exn.Message, "Function `f' expects 1 argument(s)")
      StringAssert.Contains (exn.Message, "Line")

  [<DataTestMethod>]
  [<DataRow("read()", "read")>]
  [<DataRow("pid(1)", "pid")>]
  [<DataRow("atoi()", "atoi")>]
  [<DataRow("itoa(42, 10, 1)", "itoa")>]
  member __.``builtin arity is checked statically and at runtime``
    (call: string, name: string) =
    for body in [$"return {call}"; $"{call}\n  return ()"] do
      let exn = Assert.ThrowsException<DecodingException>(fun () ->
        decode ($"submit:\n  {body}\n") |> ignore)
      StringAssert.Contains (exn.Message, $"Function `{name}' expects")
    let exn = Assert.ThrowsException<RuntimeException>(fun () -> evalUnchecked call |> ignore)
    StringAssert.Contains (exn.Message, $"Function `{name}' expects")

  [<DataTestMethod>]
  [<DataRow("atoi(\"42\")", "42")>]
  [<DataRow("atoi(\"2a\", 16)", "42")>]
  [<DataRow("itoa(42)", "\"42\"")>]
  [<DataRow("itoa(42, 16)", "\"2A\"")>]
  member __.``optional builtin arguments work in statements and expressions``
    (call: string, expected: string) =
    let source = $"submit:\n  {call}\n  return {call} = {expected}\n"
    Assert.IsTrue (Executor.isTrue (run source))

  [<DataTestMethod>]
  [<DataRow("atoi(42)")>]
  [<DataRow("itoa(42, \"16\")")>]
  member __.``optional argument lists still receive type checking`` (call: string) =
    let exn = Assert.ThrowsException<DecodingException>(fun () ->
      decode ($"submit:\n  return {call}\n") |> ignore)
    StringAssert.Contains (exn.Message, "Type mismatch")

  [<TestMethod>]
  member __.``zero and exact user arities still work`` () =
    let source =
      "fun f():\n  return 42\nfun g(x):\n  return x\nsubmit:\n  return g(f())\n"
    Assert.AreEqual (IntValue (42L, TypeAnyInt), run source)

  [<TestMethod>]
  member __.``runtime arity checks precede argument evaluation and action setup`` () =
    let typ = TypeFunction (TypeUnit, [TypeAny])
    for fn in [RegularFunction (typ, ["x"], [||]); ActionFunction (typ, ["x"], [||])] do
      let state = { Executor.emptyState () with FunctionMap = Map.ofList ["f", fn] }
      for args in [[]; [Var ("missing", None); Var ("missing", None)]] do
        let exn = Assert.ThrowsException<RuntimeException>(fun () ->
          Executor.evalCall state "f" args None |> ignore)
        StringAssert.Contains (exn.Message, "Function `f' expects 1 argument(s)")

  [<TestMethod>]
  member __.``function type arity mismatches produce a decoding error`` () =
    let exn = Assert.ThrowsException<DecodingException>(fun () ->
      TypeChecker.assertTypeConsistency None
        (TypeFunction (TypeUnit, [TypeAny])) (TypeFunction (TypeUnit, [])))
    StringAssert.Contains (exn.Message, "arities")

  // #45: dynamic conditions, all three consumers, and the nested-condition exit bug.
  [<DataTestMethod>]
  [<DataRow("if value then\n    return 1\n  else\n    return 0")>]
  [<DataRow("while value\n    break\n  return 0")>]
  [<DataRow("return if value then 1 else 0")>]
  member __.``conditions reject empty and multi-bit values`` (body: string) =
    for literal in ["\"\""; "00hs"; "41hs"] do
      let exn = Assert.ThrowsException<RuntimeException>(fun () ->
        run ($"submit:\n  bv value = {literal}\n  {body}\n") |> ignore)
      StringAssert.Contains (exn.Message, "one-bit bitvector")
      StringAssert.Contains (exn.Message, "Line")

  [<DataTestMethod>]
  [<DataRow("41hs[0:0]", 1L)>]
  [<DataRow("41hs[1:1]", 0L)>]
  member __.``one-bit conditions retain their meaning``
    (condition: string, expected: int64) =
    for body in ["if value then\n    return 1\n  else\n    return 0";
                 "while value\n    return 1\n  return 0";
                 "return if value then 1 else 0"] do
      let source = $"submit:\n  bv value = {condition}\n  {body}\n"
      Assert.AreEqual (IntValue (expected, TypeAnyInt), run source)

  [<DataTestMethod>]
  [<DataRow("break", 9L)>]
  [<DataRow("return 7", 7L)>]
  member __.``while exits skip evaluation of nested conditions``
    (exit: string, expected: int64) =
    let source =
      $"submit:\n  bv value = true\n  while (if value then true else false)\n    value := 00hs\n    {exit}\n  return 9\n"
    Assert.AreEqual (IntValue (expected, TypeAnyInt), run source)

  [<TestMethod>]
  member __.``unchecked eval rejects integer conditions`` () =
    let exn = Assert.ThrowsException<RuntimeException>(fun () ->
      evalUnchecked "if 1 then 1 else 0" |> ignore)
    StringAssert.Contains (exn.Message, "one-bit bitvector")

  // #39: verify the model against interpreter concatenation in both operand orders.
  [<TestMethod>]
  member __.``SMT concatenation places the left operand in the low bits`` () =
    let source = """
submit:
  u8 x = ()
  u8 k = 0
  solve (x . k)[0:7] = 41hs
  return (x . k)[0:7] = 41hs
"""
    Assert.IsTrue (Executor.isTrue (run source))
    let exn = Assert.ThrowsException<RuntimeException>(fun () ->
      run (source.Replace("x . k", "k . x")) |> ignore)
    StringAssert.Contains (exn.Message, "Unsatisfiable")

  // #47: cover each signed width and operator without a Cartesian product.
  [<DataTestMethod>]
  [<DataRow("i8", "x < 0:i8")>]
  [<DataRow("i16", "0:i16 > x")>]
  [<DataRow("i32", "x <= 0:i32 - 1:i32")>]
  [<DataRow("i64", "0:i64 - 1:i64 >= x")>]
  [<DataRow("i64", "0 > x")>]
  [<DataRow("i64", "x / 2 = -1")>]
  [<DataRow("i64", "x % 4 = -1")>]
  [<DataRow("i64", "x >> 3 = -1")>]
  [<DataRow("i64", "-1 < 0")>]
  [<DataRow("u8", "x > 127:u8")>]
  [<DataRow("u64", "x >> 1 = 4611686018427387904")>]
  member __.``solver signedness agrees with interpreter evaluation``
    (typ: string, constraintExpr: string) =
    let source =
      $"submit:\n  {typ} x = ()\n  solve {constraintExpr}\n  return {constraintExpr}\n"
    Assert.IsTrue (Executor.isTrue (run source), constraintExpr)

  // #48: the first and last bit patterns outside signed Int64's positive range.
  [<DataTestMethod>]
  [<DataRow("i64", "0x8000000000000000:i64", System.Int64.MinValue)>]
  [<DataRow("u64", "0xffffffffffffffff:u64", -1L)>]
  member __.``SMT models preserve the high bit for signed and unsigned integers``
    (typ: string, rhs: string, expected: int64) =
    let result = run ($"submit:\n  {typ} x = ()\n  solve x = {rhs}\n  return x\n")
    let target = if typ = "i64" then TypeInt64 else TypeUInt64
    Assert.AreEqual (IntValue (expected, target), result)
