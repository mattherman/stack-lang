module StackLang.Debugger

open System
open StackLang.Shared
open StackLang.Interpreter.Models

let optionsPrinter () =
    let mutable optionNumber = 1
    let mutable options = Map.empty<int, string * DebuggerCommand>
    let addOption = fun (text, command) ->
        options <- Map.add optionNumber (text, command) options
        optionNumber <- optionNumber + 1
    let printOptions = fun () ->
        options
        |> Map.iter (fun i (text, _) -> printfn $"{i}) {text}")
    let getResult = fun optionNumber ->
        options
        |> Map.find optionNumber
        |> snd
    {| addOption = addOption; print = printOptions; getResult = getResult |}

let getOptions (valueToExecute: Value) =
    let options = optionsPrinter ()
    options.addOption ("Step Next", DebuggerCommand.StepNext)
    match valueToExecute with
    | Word _ -> options.addOption ("Step Into", DebuggerCommand.StepInto)
    | _ -> ()
    options.addOption ("Step Previous", DebuggerCommand.StepPrevious)
    options.addOption ("Continue", DebuggerCommand.Continue)
    options

let getErrorOptions () =
    let options = optionsPrinter ()
    options.addOption ("Abort", DebuggerCommand.Continue)
    options.addOption ("Step Previous", DebuggerCommand.StepPrevious)
    options

let onExecute (valueToExecute: Value, _: Map<string, Word>, stack: Value list) =
    printfn "=== Debug ==="
    printfn $"\n--> {valueToString valueToExecute}\n"
    printStack stack
    let options = getOptions valueToExecute
    options.print ()
    printf "$ "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, value -> options.getResult value
    | _ -> DebuggerCommand.Continue

let onError (msg: string, _: Map<string, Word>, stack: Value list) =
    printfn "=== Debug ==="
    printfn $"Error: {msg}"
    printStack stack
    let options = getErrorOptions ()
    options.print ()
    printf "$ "
    let input = Console.ReadLine()
    match Int32.TryParse(input) with
    | true, value -> options.getResult value
    | _ -> DebuggerCommand.Continue

let attach interpreter =
    interpreter.Engine.AttachDebugger { OnExecute = onExecute; OnError = onError }

let detach interpreter =
    interpreter.Engine.DetachDebugger ()

let setStep interpreter value =
    if value && not (interpreter.Engine.IsDebuggerAttached ()) then
        attach interpreter
    interpreter.Engine.SetStep value
