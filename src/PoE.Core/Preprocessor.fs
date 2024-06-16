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

[<RequireQualifiedAccess>]
module internal PoE.Preprocessor

open System
open System.Text.RegularExpressions

let private parseOverwriteMacro line =
  let pattern = @"(\d+)\s+([0-9a-fA-F]+hs)\s+(\d+)\s(\d+)"
  let regex = Regex (pattern, RegexOptions.IgnoreCase)
  let m = regex.Match line
  if m.Success then
    let cnt = m.Groups.[1].ToString () |> int
    let bv = m.Groups.[2].ToString () |> BitVectorUtils.hexstrToBV
    let soff = m.Groups.[3].ToString () |> int
    let eoff = m.Groups.[4].ToString () |> int
    Overwrite (cnt, bv, soff, eoff)
  else failwith "Failed to match overwrite macro"

let private parseMacros lines =
  lines
  |> Array.fold (fun (exp, macros) (line: string) ->
    let sub = line.Substring (line.IndexOf ('#') + 1)
    if sub.StartsWith ("exploit ") then
      ExploitType.ofString (sub.Substring (8)), macros
    elif sub.StartsWith ("overwrite ") then
      exp, parseOverwriteMacro (sub.Substring (10)) :: macros
    else failwithf "Unknown macro: %s" line) (Unexploitable, [])

let run (str: string) =
  let lines = str.Split ([| "\r\n"; "\r"; "\n" |], StringSplitOptions.None)
  let macroLines, regularLines =
    lines |> Array.partition (fun l -> l.TrimStart().StartsWith "#")
  let str = String.concat Environment.NewLine regularLines
  let str = str + Environment.NewLine
  let expType, macros = parseMacros macroLines
  str, expType, macros
