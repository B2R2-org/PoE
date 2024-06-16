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

module PoE.Factory

open PoE.StringUtils
open PoE.BitVectorUtils

let declVar typ varName bytes =
  let bv = BitVector (bytes |> bytesToBV, None)
  VarStmt (typ, [ (varName, bv) ], None)

let subVar typ newVar var1 var2 =
  let var1' = Var (var1, None)
  let var2' = Var (var2, None)
  let decl = (newVar, ArithExpr (Sub (var1', var2', None)))
  VarStmt (typ, [ decl ], None)

let addVar typ newVar var1 var2 =
  let var1' = Var (var1, None)
  let var2' = Var (var2, None)
  let decl = (newVar, ArithExpr (Add (var1', var2', None)))
  VarStmt (typ, [ decl ], None)

let concatVar typ newVar var1 var2 =
  let var1' = Var (var1, None)
  let var2' = Var (var2, None)
  let decl = (newVar, Concat (var1', var2', None))
  VarStmt (typ, [ decl ] , None)

let writeVar var =
  let v = Var (var, None)
  CallStmt ("write", [ v ], None)

let getLibcFunc typ varName funName =
  let arg = BitVector (funName |> strToBV, None)
  let call = Call ("getLibcFunc", [ arg ], None)
  VarStmt (typ, [ (varName, call) ], None)

let getLibcStr typ varName str =
  let arg = BitVector (str |> strToBV, None)
  let call = Call ("getLibcStr", [ arg ], None)
  VarStmt (typ, [ (varName, call) ], None)

let genWrite bytes =
  let v = BitVector (bytesToBV bytes, None)
  CallStmt ("write", [ v ], None)

let genReadVarByLen varName len =
  let decl = varName, Call ("read", [ Int (int64 len, TypeAnyInt, None) ], None)
  VarStmt (TypeBV len, [ decl ], None)

let genReadVarByDelim varName delim =
  let bv = bytesToBV delim
  let decl = varName, Call ("read", [ BitVector (bv, None) ], None)
  VarStmt (TypeBV bv.Length, [ decl ], None)

let genReadAll varName (len: int) doTrimEnd =
  let blen = int64 len * 8L
  let soff = Int (-blen, TypeInt64, None)
  let eoff = Int (-1L, TypeInt64, None)
  let readExp = Call ("read", [ Int (-1L, TypeAnyInt, None) ], None)
  let readExp =
    if doTrimEnd then Call ("trim", [ readExp ], None) else readExp
  let subExp = Call ("substr", [ soff; eoff; readExp ], None)
  let decl = varName, subExp
  VarStmt (TypeAnyBV, [ decl ], None)

let genDelay (ms: int) =
  let v = Int (int64 ms, TypeAnyInt, None)
  CallStmt ("delay", [ v ], None)

let genPoE cbid etype stmts =
  { ChallengeBinaryID = cbid; ExploitType = etype; Decls = stmts; Macros = [] }

let genSimpleAct name body =
  ActDecl (name, [], body, None)

let genSimpleSubmit actName =
  SubmitDecl ([Return (Call (actName, [], None), None)], None)

let genTestCasePoE cbid stmts =
  let returnUnit = Return (Unit, None)
  let act = genSimpleAct "testcase" (stmts @ [returnUnit])
  let submit = genSimpleSubmit "testcase"
  genPoE cbid Unexploitable [ act; submit ]

let genTestCasePoEWithInputs cbid inputs =
  let rec helper = function
    | s::t -> [s |> genWrite] @ (helper t)
    | [] -> []
  let newStmt = helper inputs
  genTestCasePoE cbid newStmt

let genShellPoE cbid etype flagPath flagLen stmts =
  let wait = genDelay 1000
  let catflg = strToBytes ("cat " + flagPath + "\n") |> genWrite
  let readflg = genReadAll "flag" flagLen true
  let retflg = Return (Var ("flag", None), None)
  let act = genSimpleAct "exploit" (stmts @ [ wait; catflg; readflg; retflg ])
  let submit = genSimpleSubmit "exploit"
  genPoE cbid etype [ act; submit ]

let genShellCatPoE cbid etype flagLen stmts =
  let wait = genDelay 1000
  let readflg = genReadAll "flag" flagLen true
  let retflg = Return (Var ("flag", None), None)
  let act = genSimpleAct "exploit" (stmts @ [ wait; readflg; retflg ])
  let submit = genSimpleSubmit "exploit"
  genPoE cbid etype [ act; submit ]

let genLeakPoE cbid etype stmts =
  let act = genSimpleAct "exploit" stmts
  let submit = genSimpleSubmit "exploit"
  genPoE cbid etype [ act; submit ]

let genSimpleLeakPoE cbid etype payload =
  let newLine = [| 0xauy |]
  let stmts = [ if Array.isEmpty payload then () else yield genWrite payload
                yield genReadVarByDelim "flag" newLine ]
  let act = genSimpleAct "exploit" stmts
  let submit = genSimpleSubmit "exploit"
  genPoE cbid etype [ act; submit ]

let genSimpleShellPoE cbid etype flagPath flagLen payload =
  let actions = [ if Array.isEmpty payload then () else yield genWrite payload ]
  genShellPoE cbid etype flagPath flagLen actions

let addOverwriteRule poe rule =
  let s = Overwrite (rule.NthWrite, rule.SourceBV, rule.SOffset, rule.EOffset)
  { poe with Macros = s :: poe.Macros }
