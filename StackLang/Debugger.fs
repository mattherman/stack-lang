module StackLang.Debugger

open StackLang.Interpreter
open StackLang.Interpreter.Models

type ErrorState = {
    Interpreter: Interpreter
    StepPrevious: (unit -> Debugger) option
    ErrorMessage: string
}

and OkState = {
    Interpreter: Interpreter
    StepNext: unit -> Debugger
    StepPrevious: (unit -> Debugger) option
}

and Debugger =
    | Continue of OkState
    | Break of ErrorState
    | Finished of Interpreter

let getPreviousInterpreterState (interpreter: Interpreter) =
    if interpreter.Engine.State.Length > 0 then
        let dictionary, stack = List.head interpreter.Engine.State
        Some { interpreter with Dictionary = dictionary; Stack = stack }
    else
        None

let debug (tokens: string list) interpreter =
    let rec execute (interpreter, remainingTokens: string list) : Debugger =
        match remainingTokens with
        | [] -> Finished interpreter
        | tokens ->
            let result = Interpreter.next (interpreter, tokens)
            match result with
            | Ok (interpreter, remainingTokens) ->
                let stepNext = (fun () -> execute (interpreter, remainingTokens))
                let stepPrevious =
                    getPreviousInterpreterState interpreter
                    |> Option.map (fun previousInterpreter ->
                        fun () -> execute (previousInterpreter, remainingTokens)) // TODO: Need the old remaining tokens
                Continue { StepNext = stepNext; StepPrevious = stepPrevious; Interpreter = interpreter }
            | Error msg ->
                let stepPrevious =
                    getPreviousInterpreterState interpreter
                    |> Option.map (fun previousInterpreter ->
                        fun () -> execute (previousInterpreter, remainingTokens))// TODO: Need the old remaining tokens
                Break { StepPrevious = stepPrevious; ErrorMessage = msg; Interpreter = interpreter }

    execute (interpreter, tokens)