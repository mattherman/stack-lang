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
        |> Map.iter (fun i (text, _) -> printf $"\n{i}) {text}")
    let getResult = fun optionNumber ->
        options
        |> Map.tryFind optionNumber
        |> Option.map snd
    { addOption = addOption; print = printOptions; getResult = getResult }

let getOptions (valueToExecute: Value) (currentFrame: IFrame option) =
    let options = optionsPrinter ()
    options.addOption ("Step Next", DebuggerCommand.StepNext)
    match valueToExecute with
    | Word _ -> options.addOption ("Step Into", DebuggerCommand.StepInto)
    | _ -> ()
    match currentFrame with
    | Some _ -> options.addOption ("Step Out", DebuggerCommand.StepOut)
    | _ -> ()
    options.addOption ("Continue", DebuggerCommand.Continue)
    options

let getErrorOptions () =
    let options = optionsPrinter ()
    options.addOption ("Abort", DebuggerCommand.Continue)
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

let printFrame (frame: IFrame) =
    match frame.Source with
    | Word symbol ->
        printf $"\nWord:{symbol} -->"
        printWithHighlight frame.Instructions frame.InstructionIndex
    | _ ->
        printf "\nQuotation --> ["
        printWithHighlight frame.Instructions frame.InstructionIndex
        printf " ]"

let printFrameOrValue value (frame: IFrame option) =
    match frame with
    | Some frame -> printFrame frame
    | _ -> printf $"\nREPL --> {valueToString value}\n"
    printf "\n"

let printStackTrace (frames: IFrame list) =
    // Example:
    // first-word
    // ↳ second-word
    //  ↳ third-word
    frames
    |> List.rev
    |> List.iteri (fun i frame ->
        let indent = Seq.replicate i " " |> String.concat ""
        printf $"{indent}"
        if i > 0 then printf "↳ " else ()
        match frame.Source with
        | Word symbol ->
            printfn $"{symbol}"
        | _ ->
            printf "["
            printWithHighlight frame.Instructions frame.InstructionIndex
            printfn " ]")

let onExecute (valueToExecute, currentFrame, stack) =
    printStack stack
    printfn "\n=== Debug ==="
    printFrameOrValue valueToExecute currentFrame
    printf "\n"
    let options = getOptions valueToExecute currentFrame
    options.print ()
    printf "\n$> "
    Console.ReadLine() |> getCommand options

let onError (msg, frames: IFrame list, stack) =
    printStack stack
    printfn "\n=== Debug ==="
    printfn $"\nError: {msg}"
    if frames.Length > 0 then
        printfn "\nTraceback:"
        printStackTrace frames
        printFrame frames[0]
        printf "\n"
    else
        ()

    let options = getErrorOptions ()
    options.print ()
    printf "\n$> "
    Console.ReadLine() |> getCommand options

let attachDebugger interpreter =
    interpreter.Engine.AttachDebugger { OnExecute = onExecute; OnError = onError }

let detachDebugger interpreter =
    interpreter.Engine.DetachDebugger ()

let enableStepDebugging interpreter =
    if not (interpreter.Engine.IsDebuggerAttached ()) then
        attachDebugger interpreter
    interpreter.Engine.SetStep true

let disableStepDebugging interpreter =
    interpreter.Engine.SetStep false
