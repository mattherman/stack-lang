module Interpreter

open System

type Interpreter = {
    Dictionary: Map<string, Word>;
    Stack: Value list
}
and Value =
    | Literal of int
    | Word of Word
and Instructions =
    | Native of (Value list -> Result<Value list, string>)
    | Compiled of string list
and Word = {
    Symbol: string
    Instructions: Instructions
}

let printValue value =
    match value with
    | Literal l -> printfn $"%d{l}"
    | Word w -> printfn $"%s{w.Symbol}"

let getParameters count (stack: Value list) =
    if stack.Length >= count then
        Ok (List.take count stack, List.skip count stack)
    else
        Error "Stack underflow"
        
let binaryOperation name op (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, remainingStack) ->
        match (parameters[0], parameters[1]) with
        | Literal first, Literal second ->
            let newValue = Literal (op second first)
            Ok (newValue::remainingStack)
        | _ ->
            Error $"Values do not support {name}")

let NativeAdd = binaryOperation "Add" (+)
let NativeSubtract = binaryOperation "Subtract" (-)
let NativeMultiply = binaryOperation "Multiply" (*)
let NativeDivide = binaryOperation "Divide" (/)
let NativeModulus = binaryOperation "Modulus" (%)

let NativeDup (stack: Value list) =
    stack
    |> getParameters 1
    |> Result.bind (fun (parameters, remainingStack) ->
        let token = parameters[0]
        Ok (token::token::remainingStack))
    
let NativeDrop (stack: Value list) =
    stack
    |> getParameters 1
    |> Result.bind (fun (parameters, remainingStack) ->
        printValue parameters[0]
        Ok remainingStack)
    
let NativeSwap (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, remainingStack) ->
        Ok (parameters[1]::parameters[0]::remainingStack))
    
let createInterpreter () =
    let nativeWords = [
        { Symbol = "+"; Instructions = Native NativeAdd }
        { Symbol = "-"; Instructions = Native NativeSubtract }
        { Symbol = "*"; Instructions = Native NativeMultiply }
        { Symbol = "/"; Instructions = Native NativeDivide }
        { Symbol = "%"; Instructions = Native NativeModulus }
        { Symbol = "dup"; Instructions = Native NativeDup }
        { Symbol = "drop"; Instructions = Native NativeDrop }
        { Symbol = "."; Instructions = Native NativeDrop }
        { Symbol = "swap"; Instructions = Native NativeSwap }
    ]
    let vocab =
        nativeWords
        |> List.map (fun w -> (w.Symbol, w))
        |> Map.ofList
    { Dictionary = vocab; Stack = [] }

let compileWord interpreter word =
    let symbol = List.head word
    let instructions = List.tail word
    let newDictionary = interpreter.Dictionary.Add(symbol, { Symbol = symbol; Instructions = Compiled instructions })
    { interpreter with Dictionary = newDictionary }

let rec execute token interpreter =
    match interpreter.Dictionary.TryGetValue(token) with
    | true, word -> executeWord interpreter word
    | _ -> executeLiteral interpreter token

and executeWord interpreter word =
    match word.Instructions with
    | Native nativeWord ->
        nativeWord interpreter.Stack
        |> Result.map (fun newStack -> { interpreter with Stack = newStack })
    | Compiled compiledWord ->
        compiledWord |> List.fold (fun lastResult nextToken ->
            Result.bind (execute nextToken) lastResult)
            (Ok interpreter)

and executeLiteral interpreter (token: string) =
    match Int32.TryParse(token) with
    | true, number ->
        let literal = Literal number
        Ok { interpreter with Stack = literal::interpreter.Stack }
    | _ ->
        Error $"Unable to parse literal: {token}"

let rec next (tokens: string list) interpreter =
    match tokens with
    | nextToken::remainingTokens ->
        match nextToken with
        | ":" ->
            let word =
                tokens
                |> List.skip 1
                |> List.takeWhile (fun t -> not (t = ";"))
            if tokens.Length = word.Length then
                Error "Expected \";\" but got end of input"
            else
                let result = compileWord interpreter word
                let remainingTokens = tokens |> List.skip (word.Length + 2)
                next remainingTokens result
        | token ->
            let result = execute token interpreter
            result |> Result.bind (next remainingTokens)
    | [] ->
        Ok interpreter
        
let run interpreter (input: string) =
    let tokens = input.Split " " |> Array.toList
    next tokens interpreter
