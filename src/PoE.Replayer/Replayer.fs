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

module PoE.Replayer

open System

type ReplayOption = {
  ReplayLibcPath: string
  ReplayFlagInfo: (string * int) option
}
with
  static member Init () =
    { ReplayLibcPath = ""; ReplayFlagInfo = None }

let errorExit () =
  eprintfn """
Usage of the PoE replayer:

  * poe eval <PoE expression>

    Evaluate the given PoE expression, and print out the evaluated value. The
    evaluated value will be stored in a file `payload.dat` of the current
    working directory.

  * poe parse <PoE>

    Parse the given PoE file and print out the AST. When the command is not
    given, i.e., if only a single argument is given, the replayer will assume
    this command.

  * poe pp <PoE in> <PoE out>

    Reads in the file, and then outputs a canonicalized PoE file.

  * poe net [replay options] <PoE> <ip> <port>

    Replay the PoE on the remote service running on the IP and the port. The
    replay options must precede the PoE file path <PoE>.

  * poe stdin [replay options] <PoE> <bin path> [args]

    The replayer will replay the PoE on the given binary (bin path) using stdin.
    The replay options must precede the PoE file path <PoE>.

Replay options:

  -v                        : Show read/write messages during a replay session.
  -l <libc path>            : Specify libc file path. This is used by several
                              built-in functions, such as libcFuncAddr.
  -f <flag path> <flag len> : Specify local flag path and length for testing.
                              When this option is given, the replayer will
                              create a random flag of the given length in the
                              given local path to test the exploit. This option
                              is valid only with the stdin command.
"""
  exit 1

let evalAndPrint str =
  let e = Decoder.loadExprFromString str
  Executor.runExpr e

let parseAndPrint = function
  | path :: _ -> Decoder.loadPoEFromPath path |> printfn "%A"
  | [] -> eprintfn "PoE file path not given."

let canonicalizePoE = function
  | inFile :: outFile :: _ ->
    let poe, _ = Decoder.loadPoEFromPath inFile
    let str = poe |> Encoder.toString
    IO.File.WriteAllText(outFile, str) |> ignore
  | _ -> eprintfn "PoE file path(s) not given."

let rec parseReplayOptions replayOpt = function
  | "-v" :: opts ->
    Stream.debugStream <- true
    parseReplayOptions replayOpt opts
  | "-l" :: libcpath :: opts ->
    parseReplayOptions { replayOpt with ReplayLibcPath = libcpath } opts
  | "-f" :: flagpath :: flaglen :: opts ->
    let flag = Some (flagpath, Convert.ToInt32 flaglen)
    parseReplayOptions { replayOpt with ReplayFlagInfo = flag } opts
  | args -> replayOpt, args

let replayWithNetwork args =
  let opt, args = parseReplayOptions (ReplayOption.Init ()) args
  match args with
  | poePath :: ip :: port :: _ ->
    let poe, typeEnv = Decoder.loadPoEFromPath poePath
    let port = Convert.ToInt32(port)
    Executor.runPoEWithNetwork poe typeEnv ip port opt.ReplayLibcPath 1
    |> ignore
  | _ -> eprintfn "Not enough argument(s) given for replay."

let prepareFlag = function
  | Some (flagPath: string, size) ->
    let chars = "0123456789"
    let random = new Random()
    let r = seq { for i in {1..size} do yield chars.[random.Next(chars.Length)] }
    let flag = Seq.toArray r |> String
    let dirname = IO.Path.GetDirectoryName (flagPath)
    if String.IsNullOrEmpty dirname || IO.Directory.Exists dirname then ()
    else IO.Directory.CreateDirectory dirname |> ignore
    IO.File.WriteAllText (flagPath, flag)
    Some flagPath
  | None -> None

let replayWithPipe args =
  let opt, args = parseReplayOptions (ReplayOption.Init ()) args
  match args with
  | poePath :: binPath :: args ->
    if IO.File.Exists (binPath) then ()
    else failwithf "Binary file (%s) not found." binPath
    let poe, typeEnv = Decoder.loadPoEFromPath poePath
    let args = List.fold (fun acc arg -> acc + "\"" + arg + "\" ") "" args
    let args = args.TrimEnd ()
    let flagPath = prepareFlag opt.ReplayFlagInfo
    let libcPath = opt.ReplayLibcPath
    Executor.runPoEWithPipe poe typeEnv flagPath binPath libcPath args 1
    |> ignore
  | _ -> eprintfn "Not enough argument(s) given for replay."

let parseCommands (cmd: string) args =
  let args = Array.toList args
  match cmd.ToLower () with
  | "eval" -> evalAndPrint (String.concat " " args)
  | "parse" -> parseAndPrint args
  | "pp" -> canonicalizePoE args
  | "net" -> replayWithNetwork args
  | "stdin" -> replayWithPipe args
  | _ -> eprintfn "Unknown command {%s}" cmd; errorExit ()

let realMain (argv: string []) =
  match Array.length argv with
  | n when n >= 1 && (argv.[0] = "-h" || argv.[0] = "--help") -> errorExit ()
  | 1 -> parseCommands "parse" argv.[0..]
  | n when n > 1 -> parseCommands argv.[0] argv.[1..]
  | _ -> errorExit ()

[<EntryPoint>]
let main argv =
  try
    realMain argv; 0
  with
    | ParsingException (msg, annot) ->
      let line = annot |> Option.defaultValue 0
      let linestr = if line = 0 then "" else (" @ " + line.ToString ())
      eprintfn "Parsing failure%s: %s" linestr msg
      3
    | e ->
      eprintfn "Fatal error: %s" <| e.ToString ()
      eprintfn "%s" e.StackTrace
      1
