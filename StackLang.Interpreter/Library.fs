namespace StackLang.Interpreter

module Interpreter =

    open System
    open System.Text.RegularExpressions
    open Models
    open Native

    let printValue = Models.printValue

    let createInterpreter () =
        let vocab = NativeWords |> List.map (fun w -> (w.Symbol, w)) |> Map.ofList
        { Dictionary = vocab; Stack = [] }

    let push value interpreter =
        { interpreter with Stack = value :: interpreter.Stack }

    let parseInteger (token: string) =
        match Int32.TryParse(token) with
        | true, number -> Integer number |> Ok
        | _ -> Error $"Unable to parse literal: {token}"

    let parseFloat (token: string) =
        match Double.TryParse(token) with
        | true, number -> Float number |> Ok
        | _ -> Error $"Unable to parse float: {token}"

    let parseString (token: string) =
        String(token.Substring(1, token.Length - 2)) |> Ok

    let parseValueFromPrimitive (token: string) =
        match token with
        | str when Regex.IsMatch(str, "^\\\"(.*)\\\"$") -> // ^\"(.*)\"$
            parseString str
        | f when f.Contains(".") -> parseFloat f
        | _ -> parseInteger token

    let parseValue (token: string) interpreter =
        match interpreter.Dictionary.TryGetValue(token) with
        | true, word -> Word word |> Ok
        | _ -> parseValueFromPrimitive token

    let collectResults (list: Result<'a, 'e> list) : Result<'a list, 'e> =
        List.foldBack
            (fun next acc ->
                match acc with
                | Ok results ->
                    match next with
                    | Ok nextResult -> Ok(nextResult :: results)
                    | Error msg -> Error msg
                | Error msg -> Error msg)
            list
            (Ok [])

    let tokensToArray tokens =
        tokens
        |> List.map parseValueFromPrimitive
        |> collectResults
        |> Result.map (List.toArray >> Array)

    let compile tokens endToken valueFunc =
        let tokensToCompile =
            tokens |> List.skip 1 |> List.takeWhile (fun t -> not (t = endToken))

        if tokens.Length = tokensToCompile.Length then
            Error $"Expected \"{endToken}\" but got end of input"
        else
            valueFunc tokensToCompile
            |> Result.map (fun value ->
                let remainingTokens = tokens |> List.skip (tokensToCompile.Length + 2)
                (value, remainingTokens))

    let tokensToWord interpreter tokens =
        let symbol = List.head tokens
        List.tail tokens
        |> List.map (fun t -> parseValue t interpreter)
        |> collectResults
        |> Result.map (fun values ->
            { Symbol = symbol; Instructions = Compiled values })

    let compileWord interpreter tokens =
        compile tokens ";" (tokensToWord interpreter)
        |> Result.bind (fun (word, remainingTokens) ->
            let newDictionary = interpreter.Dictionary.Add(word.Symbol, word)
            ({ interpreter with Dictionary = newDictionary }, remainingTokens) |> Ok)

    let tokensToQuotation interpreter tokens =
        tokens
        |> List.map (fun t -> parseValue t interpreter)
        |> collectResults
        |> Result.map Quotation

    let compileQuotation interpreter tokens =
        compile tokens "]" (tokensToQuotation interpreter)
        |> Result.bind (fun (quotation, remainingTokens) ->
            let updatedInterpreter = push quotation interpreter
            (updatedInterpreter, remainingTokens) |> Ok)

    let compileArray interpreter tokens =
        compile tokens "}" tokensToArray
        |> Result.bind (fun (array, remainingTokens) ->
            let updatedInterpreter = push array interpreter
            (updatedInterpreter, remainingTokens) |> Ok)

    let rec execute (value: Value) interpreter =
        match value with
        | Word word -> executeWord interpreter word
        | _ -> push value interpreter |> Ok

    and executeWord interpreter word : Result<Interpreter, string> =
        match word.Instructions with
        | Native nativeWord ->
            nativeWord interpreter.Stack
            |> Result.map (fun newStack -> { interpreter with Stack = newStack })
        | Compiled compiledWord ->
            compiledWord
            |> List.fold (fun lastResult nextToken -> Result.bind (execute nextToken) lastResult) (Ok interpreter)

    let rec next (interpreter, tokens: string list) =
        match tokens with
        | nextToken :: remainingTokens ->
            match nextToken with
            | ":" -> compileWord interpreter tokens |> Result.bind next
            | "[" -> compileQuotation interpreter tokens |> Result.bind next
            | "{" -> compileArray interpreter tokens |> Result.bind next
            | token ->
                parseValue token interpreter
                |> Result.bind (fun value -> execute value interpreter)
                |> Result.bind (fun result -> next (result, remainingTokens))
        | [] -> Ok interpreter

    let run (input: string) interpreter =
        // TODO: Need more complex tokenization to handle strings with spaces
        // TODO: Need to handle nested quotations
        let tokens = input.Split " " |> Array.toList
        next (interpreter, tokens)
