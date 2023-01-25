module StackLang.Shared

open StackLang.Interpreter.Models

let printStack (stack: Value list) =
    if stack.Length > 0 then
        printfn "\n--- Data stack:"
        stack
        |> List.rev
        |> List.iter printValue
        printfn "\n"
    else
        ()

