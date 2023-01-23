﻿namespace StackLang.Interpreter

open StackLang.Interpreter.Models

module Interpreter =

    open System
    open System.Text.RegularExpressions
    open Models
    open ExecutionEngine
    open Tokenizer

    let printValue = Models.printValue

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
        let emptyWord = { Symbol = symbol; Instructions = Compiled [] }
        let interpreterWithEmptyDefinition = { interpreter with Dictionary = interpreter.Dictionary.Add(symbol, emptyWord) }
        compile (List.tail wordDefinition) ";" interpreterWithEmptyDefinition
        |> Result.map (fun (values, remainingTokens) ->
            let compiledWord = { Symbol = symbol; Instructions = Compiled values }
            let newDictionary = interpreter.Dictionary |> Map.remove symbol |> Map.add symbol compiledWord
            let interpreterWithCompiledDefinition = { interpreter with Dictionary = newDictionary }
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
        
    let rec next (interpreter, tokens: string list): Result<Interpreter * string list, string> =
        match tokens with
        | [] -> Ok (interpreter, [])
        | tokens ->
            parseNext tokens interpreter
            |> Result.bind (fun (parsedValue, interpreter, remainingTokens) ->
                match parsedValue with
                | Some value ->
                    interpreter.Engine.Execute (value, interpreter.Dictionary, interpreter.Stack)
                | None ->
                    interpreter.Stack |> Ok
                |> Result.map (fun newStack ->
                    let updatedInterpreter = { interpreter with Stack = newStack }
                    (updatedInterpreter, remainingTokens)))

    let run (tokens: string list) interpreter =
        let rec execute (interpreter, remainingTokens: string list) =
            match remainingTokens with
            | [] -> Ok (interpreter, [])
            | tokens ->
                next (interpreter, tokens)
                |> Result.bind execute
        execute (interpreter, tokens)

    let createInterpreter debug =
        let executionEngine =
            match debug with
            | true -> DebugEngine() :> IExecutionEngine
            | false -> Engine() :> IExecutionEngine
        let nativeVocabulary =
            StandardLibrary.NativeWords executionEngine
            |> List.map (fun w -> (w.Symbol, w))
            |> Map.ofList
        let interpreter = { Dictionary = nativeVocabulary; Stack = []; Engine = executionEngine }
        let compilationResult = run (tokenize StandardLibrary.CompiledWords) interpreter
        match compilationResult with
        | Ok (interpreterWithFullStandardLibrary, _) -> interpreterWithFullStandardLibrary
        | Error msg -> failwith $"Unable to initialize interpreter, failed to compile the standard library: {msg}"

