open System

[<EntryPoint>]
let main argv =
  let answer = "I Love PoE"
  Console.WriteLine ("Welcome to the world of PoE!")
  Console.WriteLine ("Type \"" + answer + "\" to solve this challenge.")
  Console.Write ("> ")
  Console.Out.Flush ()
  let s = Console.ReadLine ()
  if String.IsNullOrEmpty s || s <> answer then Console.WriteLine("Wrong"); 1
  else Console.WriteLine ("Good, this is your flag: POE_IS_AWESOME"); 0
