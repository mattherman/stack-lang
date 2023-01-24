open System
open System.IO
open StackLang.Debugger
open StackLang.Interpreter
open StackLang.Interpreter.Models
open StackLang.Interpreter.Tokenizer

let runFile file =
    let lines = File.ReadAllLines(file)
    let tokens = lines |> String.concat " " |> tokenize
    Interpreter.createInterpreter false
    |> Interpreter.run tokens
    |> ignore

let printStack (interpreter: Interpreter) =
    if interpreter.Stack.Length > 0 then
        printfn "\n--- Data stack:"
        interpreter.Stack
        |> List.iter Interpreter.printValue
        printfn "\n"

let runRepl () =
    let mutable quit = false
    let mutable interpreter = Interpreter.createInterpreter false
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
            interpreter |> printStack

let rec debugLoop debugger continueUntilError =
    match debugger with
    | Break errorState ->
        printfn "\n=== Debug ==="
        printfn $"Error: %s{errorState.ErrorMessage}"
        errorState.Interpreter |> printStack
        printfn "1) Abort"
        printfn "2) Step Previous"
        printf "$ "
        let input = Console.ReadLine()
        match input with
        | "2" ->
            match errorState.StepPrevious with
            | Some previous -> debugLoop (previous ()) false
            | None -> debugLoop (Finished errorState.Interpreter) true // TODO: Don't display this option if you can't do it
        | _ -> debugLoop (Finished errorState.Interpreter) true
    | Continue state ->
        if continueUntilError then
            debugLoop (state.StepNext ()) continueUntilError
        else
            printfn "\n=== Debug ==="
            state.Interpreter |> printStack
            printfn "1) Step Next"
            printfn "2) Step Previous"
            printfn "3) Continue"
            printf "$ "
            let input = Console.ReadLine()
            match input with
            | "1" -> 
                debugLoop (state.StepNext ()) false
            | "2" ->
                match state.StepPrevious with
                | Some previous -> debugLoop (previous ()) false
                | None -> debugLoop (Finished state.Interpreter) true // TODO: Don't display this option if you can't do it
            | _ ->
                debugLoop (state.StepNext ()) true
    | Finished interpreter ->
        printStack interpreter
        interpreter

let runDebugRepl () =
    let mutable quit = false
    let mutable step = false
    let mutable interpreter = Interpreter.createInterpreter true
    while not quit do
        printf "> "
        let input = Console.ReadLine()
        match input with
        | "#quit" ->
            quit <- true
        | "#step" ->
            step <- true
        | "#nostep" ->
            step <- false
        | _ ->
            let debugger = debug (tokenize input) interpreter
            interpreter <- debugLoop debugger (not step)

let args = Environment.GetCommandLineArgs()
match args with
| [| _; "--debug" |] -> runDebugRepl ()
| [| _; file |] -> runFile file
| _ -> runRepl ()