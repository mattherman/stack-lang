namespace StackLang.Interpreter

module Tokenizer =

    open System

    let rec takeWhileNot (terminalValue: char) (list: List<char>) =
        match list with
        | next::remaining ->
            if next = terminalValue then
                ([], remaining)
            else
                let values, unparsed = takeWhileNot terminalValue remaining
                (next::values, unparsed)
        | [] ->
            ([], [])

    let parseUntil (terminalValue: char) (stringFunc: char list -> string) (list: char list) =
        let characters, unparsed =
            list
            |> takeWhileNot terminalValue
        (stringFunc characters, unparsed)

    let rec tokenize (input: char list) : string list =
        match input with
        | [] -> []
        | [lastCharacter] ->
            [ lastCharacter |> string ]
        | nextCharacter::remainingCharacters ->
            if nextCharacter = ' ' then
                tokenize remainingCharacters
            else
                let token, unparsedCharacters =
                    match nextCharacter with
                    | '\"' ->
                        remainingCharacters
                        |> parseUntil '\"' (fun characters -> $"\"{String.Concat(characters)}\"")
                    | _ ->
                        input
                        |> parseUntil ' ' String.Concat
                let remainingTokens = tokenize unparsedCharacters
                token::remainingTokens