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

    let tokensToWord tokens =
        let symbol = List.head tokens
        let instructions = List.tail tokens

        { Symbol = symbol
          Instructions = Compiled instructions }
        |> Ok

    let compileWord interpreter tokens =
        compile tokens ";" tokensToWord
        |> Result.bind (fun (word, remainingTokens) ->
            let newDictionary = interpreter.Dictionary.Add(word.Symbol, word)
            ({ interpreter with Dictionary = newDictionary }, remainingTokens) |> Ok)
        
    let tokensToQuotation tokens =
        tokens |> Quotation |> Ok
        
    let compileQuotation interpreter tokens =
        compile tokens "]" tokensToQuotation
        |> Result.bind (fun (quotation, remainingTokens) ->
            let updatedInterpreter = push quotation interpreter
            (updatedInterpreter, remainingTokens) |> Ok)

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

    let compileArray interpreter tokens =
        compile tokens "}" tokensToArray
        |> Result.bind (fun (array, remainingTokens) ->
            let updatedInterpreter = push array interpreter
            (updatedInterpreter, remainingTokens) |> Ok)

    let rec execute (token: string) interpreter =
        match interpreter.Dictionary.TryGetValue(token) with
        | true, word -> executeWord interpreter word
        | _ ->
            let value = parseValueFromPrimitive token
            value |> Result.map (fun v -> push v interpreter)

    and executeWord interpreter word =
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
                execute token interpreter
                |> Result.bind (fun result -> next (result, remainingTokens))
        | [] -> Ok interpreter

    let run (input: string) interpreter =
        // TODO: Need more complex tokenization to handle strings with spaces
        // TODO: Need to handle nested quotations
        let tokens = input.Split " " |> Array.toList
        next (interpreter, tokens)
