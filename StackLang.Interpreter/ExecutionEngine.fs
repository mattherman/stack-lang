module StackLang.Interpreter.ExecutionEngine

open StackLang.Interpreter.Models

type DebuggerState =
    | Step of (Debugger * int)
    | StepOnError of Debugger
    | NotAttached

type Engine() =

    let mutable state: (Map<string, Word> * Value list) list = []
    let mutable debuggerState = NotAttached
    let mutable depth = 0

    let nextDebuggerState (debuggerCommand: DebuggerCommand) =
        match debuggerState with
        | Step (debugger, debuggerDepth) ->
            match debuggerCommand with
            | StepNext | StepPrevious -> Step (debugger, debuggerDepth)
            | StepInto -> Step (debugger, debuggerDepth + 1)
            | Continue -> StepOnError debugger
        | StepOnError debugger ->
            match debuggerCommand with
            | StepNext | StepPrevious -> Step (debugger, depth)
            | _ -> StepOnError debugger
        | NotAttached -> NotAttached

    let addScope () =
        depth <- depth + 1

    let removeScope () =
        depth <- depth - 1
        match debuggerState with
        | Step (debugger, debuggerDepth) when depth < debuggerDepth ->
            debuggerState <- Step (debugger, depth)
        | _ ->
            ()

    let rec execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
        match debuggerState with
        | Step (debugger, debuggerDepth) when debuggerDepth = depth ->
            debuggerState <- debugger.OnExecute (value, dictionary, stack) |> nextDebuggerState
        | _ -> ()

        let result =
            match value with
            | Word wordSymbol ->
                match dictionary.TryGetValue(wordSymbol) with
                | true, word ->
                    addScope ()
                    let result = executeInstructions (word.Instructions, dictionary, stack)
                    removeScope ()
                    result
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
                compiledWord
                |> List.fold (fun newStack nextToken ->
                    newStack
                    |> Result.bind (fun newStack ->
                        execute (nextToken, dictionary, newStack)))
                    (Ok stack)

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
                debuggerState <- Step (debugger, depth)
            | Step (debugger, _), false ->
                debuggerState <- StepOnError debugger
            | _ -> ()

        member this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
            execute (value, dictionary, stack)

        member this.ExecuteInstructions (instructions, dictionary, stack) =
            executeInstructions (instructions, dictionary, stack)