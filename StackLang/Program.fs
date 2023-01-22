open System
open System.IO
open StackLang.Interpreter
open StackLang.Interpreter.Tokenizer

let runFile file =
    let lines = File.ReadAllLines(file)
    let tokens = lines |> String.concat " " |> tokenize
    Interpreter.createInterpreter false
    |> Interpreter.run tokens
    |> ignore

let runRepl () =
    let mutable quit = false
    let mutable interpreter = Interpreter.createInterpreter true
    while not quit do
        printf "> "
        let input = Console.ReadLine()
        if input = "#quit" then
            quit <- true
        else
            let result = Interpreter.run (tokenize input) interpreter
            match result with
            | Ok (nextState, _) ->
                interpreter <- nextState
            | Error msg ->
                printfn $"Error: %s{msg}"
            if interpreter.Stack.Length > 0 then
                printfn "\n--- Data stack:"
                interpreter.Stack
                |> List.iter Interpreter.printValue
                printfn "\n"

let args = Environment.GetCommandLineArgs()
match args with
| [|_; file|] -> runFile file
| _ -> runRepl ()