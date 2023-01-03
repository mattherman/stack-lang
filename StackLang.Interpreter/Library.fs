namespace StackLang.Interpreter

module Interpreter =

    open System
    open System.Text.RegularExpressions
    open Models
    open Native

    let printValue = Models.printValue

    let createInterpreter () =
        let vocab =
            NativeWords
            |> List.map (fun w -> (w.Symbol, w))
            |> Map.ofList
        { Dictionary = vocab; Stack = [] }

    let compile tokens endToken valueFunc =
        let tokensToCompile =
            tokens
            |> List.skip 1
            |> List.takeWhile (fun t -> not (t = endToken))
        if tokens.Length = tokensToCompile.Length then
            Error $"Expected \"{endToken}\" but got end of input"
        else
            let value = valueFunc tokensToCompile
            let remainingTokens = tokens |> List.skip (tokensToCompile.Length + 2)
            Ok (value, remainingTokens)
            
    let tokensToWord tokens =
        let symbol = List.head tokens
        let instructions = List.tail tokens
        { Symbol = symbol; Instructions = Compiled instructions }
        
    let compileWord interpreter tokens =
        compile tokens ";" tokensToWord
        |> Result.bind (fun (word, remainingTokens) ->
            let newDictionary = interpreter.Dictionary.Add(word.Symbol, word)
            Ok ({ interpreter with Dictionary = newDictionary }, remainingTokens))

    let push value interpreter =
        { interpreter with Stack = value::interpreter.Stack }

    let rec execute (token: string) interpreter =
        match interpreter.Dictionary.TryGetValue(token) with
        | true, word ->
            executeWord interpreter word
        | _ ->
            match token with
            | str when Regex.IsMatch(str, "^\\\"(.*)\\\"$") -> // ^\"(.*)\"$
                executeString interpreter str
            | f when f.Contains(".") ->
                executeFloat interpreter f
            | _ ->
                executeInteger interpreter token

    and executeWord interpreter word =
        match word.Instructions with
        | Native nativeWord ->
            nativeWord interpreter.Stack
            |> Result.map (fun newStack -> { interpreter with Stack = newStack })
        | Compiled compiledWord ->
            compiledWord |> List.fold (fun lastResult nextToken ->
                Result.bind (execute nextToken) lastResult)
                (Ok interpreter)

    and executeInteger interpreter (token: string) =
        match Int32.TryParse(token) with
        | true, number ->
            interpreter |> push (Integer number) |> Ok
        | _ ->
            Error $"Unable to parse literal: {token}"

    and executeFloat interpreter (token: string) =
        match Double.TryParse(token) with
        | true, number ->
            interpreter |> push (Float number) |> Ok
        | _ ->
            Error $"Unable to parse float: {token}"

    and executeString interpreter (token: string) =
        let value = String (token.Substring(1, token.Length - 2))
        interpreter |> push value |> Ok

    let rec next (interpreter, tokens: string list) =
        match tokens with
        | nextToken::remainingTokens ->
            match nextToken with
            | ":" ->
                compileWord interpreter tokens
                |> Result.bind next
            | token ->
                execute token interpreter
                |> Result.bind (fun result -> next (result, remainingTokens))
        | [] ->
            Ok interpreter
            
    let run (input: string) interpreter =
        // TODO: Need more complex tokenization to handle strings with spaces
        let tokens = input.Split " " |> Array.toList
        next (interpreter, tokens)

