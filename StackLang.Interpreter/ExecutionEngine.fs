module StackLang.Interpreter.ExecutionEngine

open StackLang.Interpreter.Models

type DebuggerState =
    | Step of Debugger
    | StepOnError of Debugger
    | NotAttached

type Engine() =

    let mutable state: (Map<string, Word> * Value list) list = []
    let mutable debuggerState = NotAttached

    let nextDebuggerState debuggerCommand =
        match debuggerState with
        | Step debugger | StepOnError debugger ->
            match debuggerCommand with
            | StepNext | StepPrevious -> Step debugger
            | Continue -> StepOnError debugger
        | NotAttached -> NotAttached

    let rec execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
        match debuggerState with
        | Step debugger ->
            debuggerState <- debugger.OnExecute (dictionary, stack) |> nextDebuggerState
        | _ -> ()

        let result =
            match value with
            | Word wordSymbol ->
                match dictionary.TryGetValue(wordSymbol) with
                | true, word -> executeInstructions (word.Instructions, dictionary, stack)
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
        member this.SetStep enabled =
            match debuggerState, enabled with
            | StepOnError debugger, true ->
                debuggerState <- Step debugger
            | Step debugger, false ->
                debuggerState <- StepOnError debugger
            | _ -> ()

        member this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
            execute (value, dictionary, stack)

        member this.ExecuteInstructions (instructions, dictionary, stack) =
            executeInstructions (instructions, dictionary, stack)