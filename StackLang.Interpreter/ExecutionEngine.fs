module StackLang.Interpreter.ExecutionEngine

open StackLang.Interpreter.Models

type DebuggerState =
    | Step of (Debugger * int)
    | StepOnError of Debugger
    | NotAttached

type Frame(instructions: Value list) =
    let instructions = instructions
    let mutable index = 0

    interface IFrame with
        member this.Advance() =
            if not (index = instructions.Length - 1) then
                index <- index + 1
            else
                ()

        member this.Remaining() = seq {
            for i in index .. (instructions.Length - 1) do
                yield instructions[i]
        }

type Engine() =

    let mutable state: (Map<string, Word> * Value list) list = []
    let mutable debuggerState = NotAttached
    let mutable frames = []
    let getDepth () = frames.Length

    let nextDebuggerState (debuggerCommand: DebuggerCommand) =
        match debuggerState with
        | Step (debugger, debuggerDepth) ->
            match debuggerCommand with
            | StepNext | StepPrevious -> Step (debugger, debuggerDepth)
            | StepInto -> Step (debugger, debuggerDepth + 1)
            | Continue -> StepOnError debugger
        | StepOnError debugger ->
            match debuggerCommand with
            | StepNext | StepPrevious -> Step (debugger, getDepth())
            | _ -> StepOnError debugger
        | NotAttached -> NotAttached

    let addFrame instructions =
        let newFrame = Frame(instructions) :> IFrame
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

    let rec execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
        match debuggerState with
        | Step (debugger, debuggerDepth) when debuggerDepth = getDepth() ->
            debuggerState <- debugger.OnExecute (value, getCurrentFrame(), dictionary, stack) |> nextDebuggerState
        | _ -> ()

        let result =
            match value with
            | Word wordSymbol ->
                match dictionary.TryGetValue(wordSymbol) with
                | true, word ->
                    executeInstructions (word.Instructions, dictionary, stack)
                | _ -> Error $"No word named {wordSymbol} found in current vocabulary"
            | _ -> value :: stack |> Ok

        match result with
        | Ok newStack ->
            state <- (dictionary, newStack) :: state
            result
        | Error msg ->
            match debuggerState with
            | StepOnError debugger ->
                debuggerState <- debugger.OnError (msg, dictionary, stack) |> nextDebuggerState
                result // TODO: Handle step previous
            | _ ->
                result

    and executeInstructions (instructions, dictionary, stack) =
        let result =
            match instructions with
            | Native nativeWord ->
                nativeWord dictionary stack
            | Compiled compiledWord ->
                let frame = addFrame compiledWord
                let result =
                    compiledWord
                    |> List.fold (fun newStack nextToken ->
                        frame.Advance()
                        newStack
                        |> Result.bind (fun newStack ->
                            execute (nextToken, dictionary, newStack)))
                        (Ok stack)
                removeFrame ()
                result

        result
        |> Result.iter (fun newStack ->
            state <- (dictionary, newStack) :: state)
        
        result

    interface IExecutionEngine with
        member this.State = state

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

        member this.ExecuteInstructions (instructions, dictionary, stack) =
            executeInstructions (instructions, dictionary, stack)