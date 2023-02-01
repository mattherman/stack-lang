module StackLang.Shared

open StackLang.Interpreter.Models
open System

let printStack (stack: Value list) =
    if stack.Length > 0 then
        printfn "\n--- Data stack:"
        stack
        |> List.rev
        |> List.iter printValue
        printf "\n"
    else
        printf "\n"

let parseInt (str: string) =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | _ -> None

module Option =
    let ifNone func opt =
        match opt with
        | None -> func ()
        | Some _ -> ()