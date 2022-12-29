namespace StackLang.Interpreter

module Interpreter =

    open System
    open Models
    open Native

    let printValue = Models.printValue

    let createInterpreter () =
        let vocab =
            NativeWords
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

