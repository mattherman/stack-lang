namespace StackLang.Interpreter

open StackLang.Interpreter.Models

module ExecutionEngine =

    let execute executeInstructionsFunc (value: Value) (dictionary: Map<string, Word>) (stack: Value list) =
        match value with
        | Word wordSymbol ->
            match dictionary.TryGetValue(wordSymbol) with
            | true, word -> executeInstructionsFunc word.Instructions dictionary stack
            | _ -> Error $"No word named {wordSymbol} found in current vocabulary"
        | _ -> value :: stack |> Ok
        
    let executeInstructions executeFunc instructions (dictionary: Map<string, Word>) (stack: Value list) =
        match instructions with
        | Native nativeWord ->
            nativeWord dictionary stack
        | Compiled compiledWord ->
            compiledWord
            |> List.fold (fun newStack nextToken -> Result.bind (executeFunc nextToken dictionary) newStack) (Ok stack)

    type Engine() =

        interface IExecutionEngine with
            member this.Execute (value: Value) (dictionary: Map<string, Word>) (stack: Value list) =
                execute (this :> IExecutionEngine).ExecuteInstructions value dictionary stack

            member this.ExecuteInstructions instructions dictionary stack =
                executeInstructions (this :> IExecutionEngine).Execute instructions dictionary stack