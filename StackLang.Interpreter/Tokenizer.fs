module StackLang.Interpreter.Tokenizer

open System

let rec takeWhileNot (predicate: char -> bool) (list: List<char>) =
    match list with
    | next::remaining ->
        match next with
        | '\\' ->
            match remaining with
            | escapedCharacter::newRemaining ->
                let values, unparsed = takeWhileNot predicate newRemaining
                (escapedCharacter::values, unparsed)
            | _ ->
                (remaining, [])
        | _ ->
            if predicate next then
                ([], remaining)
            else
                let values, unparsed = takeWhileNot predicate remaining
                (next::values, unparsed)
    | [] ->
        ([], [])

let parseUntil (predicate: char -> bool) (stringFunc: char list -> string) (list: char list) =
    let characters, unparsed =
        list
        |> takeWhileNot predicate
    (stringFunc characters, unparsed)

let tokenize (input: string) : string list =
    let rec tokenize (input: char list) : string list =
        match input with
        | [] -> []
        | [lastCharacter] ->
            if Char.IsWhiteSpace(lastCharacter) then
                []
            else
                [ lastCharacter |> string ]
        | nextCharacter::remainingCharacters ->
            if Char.IsWhiteSpace(nextCharacter) then
                tokenize remainingCharacters
            else
                let token, unparsedCharacters =
                    match nextCharacter with
                    | '\"' ->
                        remainingCharacters
                        |> parseUntil (fun c -> c = '\"') (fun characters -> $"\"{String.Concat(characters)}\"")
                    | _ ->
                        input
                        |> parseUntil Char.IsWhiteSpace String.Concat
                let remainingTokens = tokenize unparsedCharacters
                token::remainingTokens

    tokenize (input |> Seq.toList)
