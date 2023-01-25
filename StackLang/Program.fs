open System
open System.IO
open StackLang.Shared
open StackLang.Debugger
open StackLang.Interpreter
open StackLang.Interpreter.Tokenizer

let runFile file =
    let lines = File.ReadAllLines(file)
    let tokens = lines |> String.concat " " |> tokenize
    Interpreter.createInterpreter ()
    |> Interpreter.run tokens
    |> ignore

let runRepl () =
    let mutable quit = false
    let mutable interpreter = Interpreter.createInterpreter ()
    attach interpreter
    while not quit do
        printf "> "
        let input = Console.ReadLine()
        match input with
        | "#quit" ->
            quit <- true
        | "#debug" ->
            attach interpreter
        | "#nodebug" ->
            detach interpreter
        | "#step" ->
            setStep interpreter true
        | "#nostep" ->
            setStep interpreter false
        | input ->
            let result = Interpreter.run (tokenize input) interpreter
            match result with
            | Ok (nextState, _) ->
                interpreter <- nextState
            | Error msg ->
                printfn $"Error: %s{msg}"
            interpreter.Stack |> printStack

let args = Environment.GetCommandLineArgs()
match args with
| [| _; file |] -> runFile file
| _ -> runRepl ()