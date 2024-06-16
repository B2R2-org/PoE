open System

let [<Literal>] flagPath = "flag.txt"

let rand n =
  let r = Random()
  let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|]])
  let sz = Array.length chars in
  String (Array.init n (fun _ -> chars.[r.Next sz]))

[<EntryPoint>]
let main argv =
  let challenge = rand 10
  Console.WriteLine ("Welcome to the world of PoE!")
  Console.WriteLine ("Type \"" + challenge + "\" to solve this challenge.")
  Console.Write ("> ")
  Console.Out.Flush ()
  let s = Console.ReadLine ()
  if String.IsNullOrEmpty s || s <> challenge then Console.WriteLine("Wrong"); 1
  else
    IO.File.ReadAllText flagPath |> Console.WriteLine
    0
