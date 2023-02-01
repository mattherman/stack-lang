open System
open System.IO
open StackLang.Shared
open StackLang.Debugger
open StackLang.Interpreter
open StackLang.Interpreter.Tokenizer

let printHelp () =
    printfn "Enter input to execute or a command."
    printfn "\nThe following commands are available:"
    printfn "#quit - exits the interpreter"
    printfn "#debug - attaches a debugger (attached by default)"
    printfn "#nodebug - detaches the debuggger"
    printfn "#step - attaches a debugger and enables step debugging"
    printfn "#nostep - disables step debugging"
    printfn "#help - displays this help information"
    printfn "\nhttps://github.com/mattherman/stack-lang"

let runFile file =
    let lines = File.ReadAllLines(file)
    let tokens = lines |> String.concat " " |> tokenize
    Interpreter.createInterpreter ()
    |> Interpreter.run tokens
    |> ignore

let runRepl () =
    let mutable quit = false
    let mutable interpreter = Interpreter.createInterpreter ()
    attachDebugger interpreter

    printfn "Interpreter started. Enter #help for more information."
    while not quit do
        printf "\n> "
        let input = Console.ReadLine()
        match input with
        | "#help" ->
            printHelp ()
        | "#quit" ->
            quit <- true
            printfn "Goodbye!"
        | "#debug" ->
            attachDebugger interpreter
            printfn "Debugger attached."
        | "#nodebug" ->
            detachDebugger interpreter
            printfn "Debugger detached."
        | "#step" ->
            enableStepDebugging interpreter
            printfn "Step debugging enabled."
        | "#nostep" ->
            disableStepDebugging interpreter
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
| [| _; "--help" |] | [| _; "-h" |] -> printHelp ()
| [| _; file |] -> runFile file
| _ -> runRepl ()