module StackLang.Debugger

open System
open StackLang.Shared
open StackLang.Interpreter.Models

type OptionsPrinter = {
    addOption: string * DebuggerCommand -> unit
    print: unit -> unit
    getResult: int -> DebuggerCommand option
}

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
        |> Map.tryFind optionNumber
        |> Option.map snd
    { addOption = addOption; print = printOptions; getResult = getResult }

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

let getCommand (options: OptionsPrinter) (str: string) =
    str
    |> parseInt
    |> Option.bind options.getResult
    |> Option.defaultValue DebuggerCommand.Continue

let printWithHighlight values indexToHighlight =
    values
    |> Seq.iteri (fun i value ->
        if not (i = indexToHighlight) then
            Console.ForegroundColor <- ConsoleColor.DarkGray
        else
            ()
        printf $" {valueToString value}"
        Console.ResetColor())

let printFrame value (frame: IFrame option) =
    match frame with
    | Some frame ->
        match frame.Source with
        | Word symbol ->
            printf $"\nWord:{symbol} -->"
            printWithHighlight frame.Instructions frame.InstructionIndex
        | _ ->
            printf "\nQuotation --> ["
            printWithHighlight frame.Instructions frame.InstructionIndex
            printf " ]"
    | _ ->
        printf $"\nREPL --> {valueToString value}\n"

    printf "\n"

let printStackTrace (frames: IFrame list) =
    // Example:
    // first-word
    // ↳ second-word
    //  ↳ third-word
    frames
    |> List.rev
    |> List.iteri (fun i frame ->
        let tabs = Seq.replicate i " " |> String.concat ""
        printf $"{tabs}"
        if i > 0 then printf "↳ " else ()
        match frame.Source with
        | Word symbol ->
            printfn $"{symbol}"
        | _ ->
            printf "["
            printWithHighlight frame.Instructions frame.InstructionIndex
            printf " ]\n")
    printf "\n"

let onExecute (valueToExecute: Value, currentFrame: IFrame option, _: Map<string, Word>, stack: Value list) =
    printStack stack
    printfn "\n=== Debug ==="
    printFrame valueToExecute currentFrame
    printf "\n"
    let options = getOptions valueToExecute
    options.print ()
    printf "$> "
    Console.ReadLine() |> getCommand options

let onError (msg: string, frames: IFrame list, _: Map<string, Word>, stack: Value list) =
    printfn $"\nError: {msg}"
    printStack stack
    printfn "\n=== Debug ==="
    printfn "\nTraceback:"
    printStackTrace frames
    let options = getErrorOptions ()
    // TODO: Allow printing current word definition
    // TODO: Allow editing the stack?
    options.print ()
    printf "$> "
    Console.ReadLine() |> getCommand options

let attach interpreter =
    interpreter.Engine.AttachDebugger { OnExecute = onExecute; OnError = onError }

let detach interpreter =
    interpreter.Engine.DetachDebugger ()

let setStep interpreter value =
    if value && not (interpreter.Engine.IsDebuggerAttached ()) then
        attach interpreter
    interpreter.Engine.SetStep value
