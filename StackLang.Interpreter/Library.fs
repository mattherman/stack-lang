namespace StackLang.Interpreter

module Interpreter =

    open System
    open System.Text.RegularExpressions
    open Models
    open ExecutionEngine
    open Tokenizer

    let printValue = Models.printValue

    let push value interpreter =
        { interpreter with Stack = value :: interpreter.Stack }

    let define word interpreter =
        let key = word.Symbol
        let newDictionary =
            if Map.containsKey key interpreter.Dictionary then
                interpreter.Dictionary |> Map.remove key |> Map.add key word
            else
                interpreter.Dictionary |> Map.add key word
        { interpreter with Dictionary = newDictionary }

    let isWordDefined symbol interpreter =
        Map.containsKey symbol interpreter.Dictionary

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

    let parseBoolean (token: string) =
        match token with
        | "t" -> Boolean true |> Ok
        | "f" -> Boolean false |> Ok
        | _ -> Error $"Unable to parse boolean: {token}"

    let parseValueFromPrimitive (token: string) =
        match token with
        | b when Regex.IsMatch(b, "^(t|f)$") ->
            parseBoolean b
        | s when Regex.IsMatch(s, "^\\\"(.*)\\\"$") -> // ^\"(.*)\"$
            parseString s
        | f when f.Contains(".") ->
            parseFloat f
        | i ->
            parseInteger i

    let parseValue (token: string) interpreter =
        match (isWordDefined token interpreter) with
        | true -> Word token |> Ok
        | _ -> parseValueFromPrimitive token

    let rec parseNext (tokens: string list) interpreter : Result<Value option * Interpreter * string list, string> =
        match List.head tokens with
        | ":" -> compileWord interpreter tokens
        | _ ->
            parseNextValue tokens interpreter
            |> Result.map (fun (value, remainingTokens) ->
                (Some value, interpreter, remainingTokens))

    and parseNextValue (tokens: string list) interpreter : Result<Value * string list, string> =
        match List.head tokens with
        | "[" -> compileQuotation interpreter tokens
        | "{" -> compileArray interpreter tokens
        | token ->
            parseValue token interpreter
            |> Result.map (fun value -> (value, List.tail tokens))

    and compile (tokens: string list) endToken interpreter : Result<Value list * string list, string> =
        match tokens with
        | token::remainingTokens ->
            if token = endToken then
                Ok ([], remainingTokens)
            else
                parseNextValue tokens interpreter
                |> Result.bind (fun (value, remainingTokens) ->
                    compile remainingTokens endToken interpreter
                    |> Result.map (fun (values, remainingTokens) ->
                        (value :: values, remainingTokens)))
        | [] -> Error $"Expected \"{endToken}\" but got end of input"

    and compileWord interpreter tokens : Result<Value option * Interpreter * string list, string> =
        let wordDefinition = List.tail tokens
        let symbol = List.head wordDefinition
        let body = List.tail wordDefinition
        let emptyWord = { Symbol = symbol; Instructions = Compiled [] }
        let interpreterWithEmptyDefinition = define emptyWord interpreter
        compile body ";" interpreterWithEmptyDefinition
        |> Result.map (fun (values, remainingTokens) ->
            let compiledWord = { Symbol = symbol; Instructions = Compiled values }
            let interpreterWithCompiledDefinition = define compiledWord interpreter
            (None, interpreterWithCompiledDefinition, remainingTokens))

    and compileQuotation interpreter tokens =
        compile (List.tail tokens) "]" interpreter
        |> Result.bind (fun (values, remainingTokens) ->
            (Quotation values, remainingTokens) |> Ok)

    and compileArray interpreter tokens =
        compile (List.tail tokens) "}" interpreter
        |> Result.bind (fun (values, remainingTokens) ->
            let array = values |> List.toArray |> Array
            (array, remainingTokens) |> Ok)
        
    let rec next (interpreter, tokens: string list) =
        match tokens with
        | [] -> Ok interpreter
        | tokens ->
            parseNext tokens interpreter
            |> Result.bind (fun (parsedValue, interpreter, remainingTokens) ->
                match parsedValue with
                | Some value ->
                    execute value interpreter.Dictionary interpreter.Stack
                | None ->
                    interpreter.Stack |> Ok
                |> Result.bind (fun newStack ->
                    let updatedInterpreter = { interpreter with Stack = newStack }
                    next (updatedInterpreter, remainingTokens)))

    let run (input: string) interpreter =
        let tokens = tokenize (Seq.toList input)
        next (interpreter, tokens)

    let createInterpreter () =
        let nativeVocabulary =
            StandardLibrary.NativeWords
            |> List.map (fun w -> (w.Symbol, w))
            |> Map.ofList
        let interpreter = { Dictionary = nativeVocabulary; Stack = [] }
        let compilationResult = run StandardLibrary.CompiledWords interpreter
        match compilationResult with
        | Ok interpreterWithFullStandardLibrary -> interpreterWithFullStandardLibrary
        | Error msg -> failwith $"Unable to initialize interpreter, failed to compile the standard library: {msg}"

