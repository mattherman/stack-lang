open System
open System.IO
open StackLang.Interpreter

let runFile file =
    let lines = File.ReadAllLines(file)
    let input = String.concat " " lines
    Interpreter.createInterpreter ()
    |> Interpreter.run input
    |> ignore

let runRepl () =
    let mutable quit = false
    let mutable interpreter = Interpreter.createInterpreter ()
    while not quit do
        printf "> "
        let input = Console.ReadLine()
        if input = "#quit" then
            quit <- true
        else
            let result = Interpreter.run input interpreter
            match result with
            | Ok nextState ->
                interpreter <- nextState
            | Error msg ->
                printfn $"Error: %s{msg}"
            printfn "\n--- Data stack:"
            interpreter.Stack
            |> List.iter Interpreter.printValue
            printfn "\n"

let args = Environment.GetCommandLineArgs()
match args with
| [|_; file|] -> runFile file
| _ -> runRepl ()