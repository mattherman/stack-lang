module StackLang.Interpreter.ExecutionEngine

open Models

let rec execute (value: Value) (dictionary: Map<string, Word>) (stack: Value list) =
    match value with
    | Word wordSymbol ->
        match dictionary.TryGetValue(wordSymbol) with
        | true, word -> executeInstructions word.Instructions dictionary stack
        | _ -> Error $"No word named {wordSymbol} found in current vocabulary"
    | _ -> value :: stack |> Ok
        
and executeInstructions instructions dictionary stack =
    match instructions with
    | Native nativeWord ->
        nativeWord dictionary stack
    | Compiled compiledWord ->
        compiledWord
        |> List.fold (fun newStack nextToken -> Result.bind (execute nextToken dictionary) newStack) (Ok stack)