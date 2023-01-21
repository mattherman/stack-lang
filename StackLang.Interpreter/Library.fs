namespace StackLang.Interpreter

module Interpreter =

    open System
    open System.Text.RegularExpressions
    open Models
    open Native
    open ExecutionEngine

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
        match interpreter.Dictionary.ContainsKey(token) with
        | true-> Word token |> Ok
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
        compile (List.tail wordDefinition) ";" interpreter
        |> Result.map (fun (values, remainingTokens) ->
            let word = { Symbol = symbol; Instructions = Compiled values }
            let newDictionary = interpreter.Dictionary.Add(symbol, word)
            let updatedInterpreter = { interpreter with Dictionary = newDictionary }
            (None, updatedInterpreter, remainingTokens))

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
        // TODO: Need more complex tokenization to handle strings with spaces
        let tokens = input.Split " " |> Array.toList
        next (interpreter, tokens)
