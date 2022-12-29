open System
open System.IO
open Interpreter

let runFile file =
    let lines = File.ReadAllLines(file)
    let input = String.concat " " lines
    run (createInterpreter ()) input |> ignore

let runRepl () =
    let mutable quit = false
    let mutable interpreter = createInterpreter ()
    while not quit do
        printf "> "
        let input = Console.ReadLine()
        if input = "#quit" then
            quit <- true
        else
            let result = run interpreter input
            match result with
            | Ok nextState ->
                interpreter <- nextState
            | Error msg ->
                printfn $"Error: %s{msg}"
            printfn "\n--- Data stack:"
            interpreter.Stack
            |> List.iter printValue
                

let args = Environment.GetCommandLineArgs()
match args with
| [|_; file|] -> runFile file
| _ -> runRepl ()