module StackLang.Debugger

open StackLang.Shared
open StackLang.Interpreter.Models

let onExecute (_: Map<string, Word>, stack: Value list) =
    printfn "=== Debug ==="
    printStack stack
    printfn "1) Step Next"
    printfn "2) Continue"
    printfn "$ "
    let input = System.Console.ReadLine()
    match input with
    | "1" -> DebuggerCommand.StepNext
    | _ -> DebuggerCommand.Continue

let onError (msg: string, _: Map<string, Word>, stack: Value list) =
    printfn "=== Debug ==="
    printfn $"Error: {msg}"
    printStack stack
    printfn "1) Abort"
    printfn "$ "
    let input = System.Console.ReadLine()
    match input with
    | _ -> DebuggerCommand.Continue

let attach interpreter =
    interpreter.Engine.AttachDebugger { OnExecute = onExecute; OnError = onError }

let detach interpreter =
    interpreter.Engine.DetachDebugger ()

let setStep interpreter value =
    interpreter.Engine.SetStep value
