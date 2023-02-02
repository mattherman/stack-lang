open System
open System.IO
open StackLang.Interpreter.Models
open StackLang.Shared
open StackLang.Debugger
open StackLang.Interpreter
open StackLang.Interpreter.Tokenizer

let printHelp () =
    printfn "Enter input to execute or a command."
    printfn "\nThe following commands are available:"
    printfn "#quit - exits the interpreter"
    printfn "#vocab - displays all words defined in the vocabulary"
    printfn "#define <symbol> - displays the definition of the word associated with <symbol>"
    printfn "#debug - attaches a debugger (attached by default)"
    printfn "#nodebug - detaches the debuggger"
    printfn "#step - attaches a debugger and enables step debugging"
    printfn "#nostep - disables step debugging"
    printfn "#help - displays this help information"
    printfn "\nhttps://github.com/mattherman/stack-lang"

let printVocabulary (dictionary: Map<string, Word>) =
    dictionary
    |> Map.keys
    |> Seq.sort
    |> Seq.iter (fun word -> printfn $"{word}")

let printDefinition (symbol: string) (dictionary: Map<string, Word>) =
    match dictionary.TryGetValue(symbol) with
    | true, word ->
        printf $": {word.Symbol} "
        match word.Instructions with
        | Native _ ->
            printf "<native>"
        | Compiled instructions ->
            let tokens =
                instructions
                |> List.map valueToString
                |> String.concat " "
            printf $"{tokens}"
        printfn " ;"
    | _ -> printfn $"No symbol '{symbol}' defined in the vocabulary."

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
        | "#vocab" ->
            printVocabulary interpreter.Dictionary
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
        | input when input.StartsWith("#define") ->
            match input.Split(" ") with
            | [| _; symbol |] ->
                printDefinition symbol interpreter.Dictionary
            | _ ->
                printfn "Usage: #define <symbol>"
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