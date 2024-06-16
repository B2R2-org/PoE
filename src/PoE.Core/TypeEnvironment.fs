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

open System.Collections.Generic

/// Type checking environment.
type TypeEnvironment (builtInFuncs: (string * ExprType * ExprType list) []) =
  let symbTabs = Stack<Map<string, ExprType>>()

  do builtInFuncs
     |> Array.fold (fun map (name, retType, paramTypes) ->
       let funcType = TypeFunction (retType, paramTypes)
       Map.add name funcType map) Map.empty
     |> fun map -> symbTabs.Push (map)

  static member Err annot msg = Exception.err DecodingException annot msg

  member __.AddSymbolType name typ annot =
    let m = symbTabs.Pop ()
    if Map.containsKey name m then
      TypeEnvironment.Err annot ("Duplicate use of the symbol `" + name + "'")
    else
      symbTabs.Push (Map.add name typ m)

  member __.FindSymbolType name annot =
    let m = symbTabs.Peek ()
    match Map.tryFind name m with
    | Some typ -> typ
    | None -> TypeEnvironment.Err annot ("Unknown symbol `" + name + "'")

  member __.EnterScope () =
    let m = symbTabs.Peek ()
    symbTabs.Push (m)

  member __.ExitScope () =
    symbTabs.Pop () |> ignore

  member __.Dump () =
    symbTabs
    |> Seq.head
    |> Map.iter (fun n t -> printfn "%s => %A" n t)
