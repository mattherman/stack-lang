module Interpreter

open System

type CompileState =
    | NotCompiling
    | Defining
    | Compiling of word:string

type Interpreter = {
    Dictionary: Map<string, Word>;
    Stack: Value list
    Compiling: bool;
}
and Value =
    | Literal of int
    | Word of Word
and Instructions =
    | Native of (Value list -> Result<Value list, string>)
    | Compiled of string[]
and Word = {
    Symbol: string
    Instructions: Instructions
}

let split count list =
    (List.take count list, List.skip count list)

let getParameters count (stack: Value list) =
    if stack.Length >= count then
        Ok (stack |> split count)
    else
        Error "Stack underflow"
        
let NativeAdd (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, remainingStack) ->
        match (parameters[0], parameters[1]) with
        | Literal first, Literal second ->
            let newValue = Literal (second + first)
            Ok (newValue::remainingStack)
        | _ ->
            Error "Values do not support (+)")

let NativeSubtract (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, stack) ->
        match (parameters[0], parameters[1]) with
        | Literal first, Literal second ->
            let newValue = Literal (second - first)
            Ok (newValue::stack)
        | _ ->
            Error "Values do not support (-)")
    
let createInterpreter () =
    let nativeWords = [
        ("+", { Symbol = "+"; Instructions = Native NativeAdd })
        ("-", { Symbol = "-"; Instructions = Native NativeSubtract })
    ]
    let vocab = nativeWords |> Map.ofList
    { Dictionary = vocab; Stack = []; Compiling = false }
    
let executeWord interpreter word =
    printfn $"Executing word: %s{word.Symbol}"
    match word.Instructions with
    | Native nativeWord ->
        nativeWord interpreter.Stack
        |> Result.map (fun newStack -> { interpreter with Stack = newStack })
    | Compiled compiledWord ->
        printfn $" => %A{compiledWord}"
        Ok interpreter
    
let executeLiteral interpreter (token: string) =
    match Int32.TryParse(token) with
    | true, number ->
        let literal = Literal number
        Ok { interpreter with Stack = literal::interpreter.Stack }
    | _ ->
        Error $"Unable to parse literal: {token}"

let execute interpreter token =
    match interpreter.Dictionary.TryGetValue(token) with
    | true, word -> executeWord interpreter word
    | _ -> executeLiteral interpreter token

let compile interpreter token =
    Ok interpreter

let next interpreter token =
    match token with
    | ":" -> Ok { interpreter with Compiling = true }
    | ";" -> Ok { interpreter with Compiling = false }
    | token ->
        if interpreter.Compiling then
            compile interpreter token
        else
            execute interpreter token

let run interpreter (input: string) =
    input.Split " "
    |> Array.toList
    |> List.fold (fun lastResult nextToken ->
        match lastResult with
        | Error msg -> Error msg
        | Ok interpreter ->
            next interpreter nextToken) (Ok interpreter)