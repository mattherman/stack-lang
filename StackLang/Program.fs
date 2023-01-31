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

    printfn "Interpreter started. Enter #help for more information."
    while not quit do
        printf "\n> "
        let input = Console.ReadLine()
        match input with
        | "#help" ->
            printfn "Enter input to execute or a command."
            printfn "\nThe following commands are available:"
            printfn "#quit - exits the interpreter"
            printfn "#debug - attaches a debugger (attached by default)"
            printfn "#nodebug - detaches the debuggger"
            printfn "#step - attaches a debugger and enables step debugging"
            printfn "#nostep - disables step debugging"
            printfn "#help - displays this help information"
            printfn "\nhttps://github.com/mattherman/stack-lang"
        | "#quit" ->
            quit <- true
            printfn "Goodbye!"
        | "#debug" ->
            attach interpreter
            printfn "Debugger attached."
        | "#nodebug" ->
            detach interpreter
            printfn "Debugger detached."
        | "#step" ->
            setStep interpreter true
            printfn "Step debugging enabled."
        | "#nostep" ->
            setStep interpreter false
            printfn "Step debugging disabled."
        | input when input.StartsWith("#") ->
            printfn "Invalid command."
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