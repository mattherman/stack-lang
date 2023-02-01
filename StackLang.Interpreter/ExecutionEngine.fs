module StackLang.Interpreter.ExecutionEngine

open StackLang.Interpreter.Models

type DebuggerState =
    | Step of (Debugger * int)
    | StepOnError of Debugger
    | NotAttached

type Frame(instructions: Value list, source: Value) =
    let mutable index = 0

    interface IFrame with
        member this.Advance() =
            if not (index = instructions.Length - 1) then
                index <- index + 1
            else
                ()

        member this.InstructionIndex = index

        member this.Instructions = instructions

        member this.CurrentInstruction = instructions[index]

        member this.RemainingInstructions() =
            Seq.toList (seq {
                for i in index .. (instructions.Length - 1) do
                    yield instructions[i]
            })

        member this.Source = source

type Engine() =

    let mutable debuggerState = NotAttached
    let mutable lastError = None
    let mutable frames = []
    let getDepth () = frames.Length

    let nextDebuggerState (debuggerCommand: DebuggerCommand) =
        match debuggerState with
        | Step (debugger, debuggerDepth) ->
            match debuggerCommand with
            | StepNext -> Step (debugger, debuggerDepth)
            | StepInto -> Step (debugger, debuggerDepth + 1)
            | StepOut -> Step (debugger, debuggerDepth - 1)
            | Continue -> StepOnError debugger
        | StepOnError debugger ->
            match debuggerCommand with
            | StepNext -> Step (debugger, getDepth())
            | _ -> StepOnError debugger
        | NotAttached -> NotAttached

    let addFrame source instructions =
        let newFrame = Frame(instructions, source) :> IFrame
        frames <- newFrame :: frames
        newFrame

    let removeFrame () =
        frames <- List.tail frames
        match debuggerState with
        | Step (debugger, debuggerDepth) when getDepth() < debuggerDepth ->
            debuggerState <- Step (debugger, getDepth())
        | _ ->
            ()

    let getCurrentFrame () =
        match frames with
        | [] -> None
        | _ -> (List.head frames) |> Some

    let rec execute (value, dictionary: Map<string, Word>, stack) =
        lastError <- None

        match debuggerState with
        | Step (debugger, debuggerDepth) when debuggerDepth = getDepth() ->
            debuggerState <- debugger.OnExecute (value, getCurrentFrame(), stack) |> nextDebuggerState
        | _ -> ()

        let result =
            match value with
            | Word wordSymbol ->
                match dictionary.TryGetValue(wordSymbol) with
                | true, word ->
                    executeInstructions (value, word.Instructions, dictionary, stack)
                | _ -> Error $"No word named {wordSymbol} found in current vocabulary"
            | _ -> value :: stack |> Ok

        match result, lastError with
        | Error msg, None ->
            lastError <- Some msg
            match debuggerState with
            | Step (debugger, _) ->
                debuggerState <- debugger.OnError (msg, frames, stack) |> nextDebuggerState
            | StepOnError debugger ->
                debuggerState <- debugger.OnError (msg, frames, stack) |> nextDebuggerState
            | _ -> ()
            Error msg
        | _ -> result

    and executeInstructions (source, instructions, dictionary, stack) =
        match instructions with
        | Native nativeWord ->
            nativeWord dictionary stack
        | Compiled compiledWord ->
            let frame = addFrame source compiledWord
            let result =
                compiledWord
                |> List.fold (fun newStack nextToken ->
                    newStack
                    |> Result.bind (fun newStack ->
                        let result = execute (nextToken, dictionary, newStack)
                        frame.Advance()
                        result))
                    (Ok stack)
            removeFrame ()
            result

    interface IExecutionEngine with
        member this.AttachDebugger debuggerToAttach =
            debuggerState <- StepOnError debuggerToAttach

        member this.DetachDebugger () =
            debuggerState <- NotAttached

        member this.IsDebuggerAttached () =
            match debuggerState with
            | NotAttached -> false
            | _ -> true

        member this.SetStep enabled =
            match debuggerState, enabled with
            | StepOnError debugger, true ->
                debuggerState <- Step (debugger, getDepth())
            | Step (debugger, _), false ->
                debuggerState <- StepOnError debugger
            | _ -> ()

        member this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
            execute (value, dictionary, stack)

        member this.ExecuteInstructions (source, instructions, dictionary, stack) =
            executeInstructions (source, instructions, dictionary, stack)