module StackLang.Interpreter.Native

open Models

let getParameters count (stack: Value list) =
    if stack.Length >= count then
        Ok (List.take count stack, List.skip count stack)
    else
        Error "Stack underflow"
        
let binaryOperation name op (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, remainingStack) ->
        match (parameters[0], parameters[1]) with
        | Literal first, Literal second ->
            let newValue = Literal (op second first)
            Ok (newValue::remainingStack)
        | _ ->
            Error $"Values do not support {name}")

let NativeAdd = binaryOperation "Add" (+)
let NativeSubtract = binaryOperation "Subtract" (-)
let NativeMultiply = binaryOperation "Multiply" (*)
let NativeDivide = binaryOperation "Divide" (/)
let NativeModulus = binaryOperation "Modulus" (%)

let NativeDup (stack: Value list) =
    stack
    |> getParameters 1
    |> Result.bind (fun (parameters, remainingStack) ->
        let token = parameters[0]
        Ok (token::token::remainingStack))
    
let NativeDrop (stack: Value list) =
    stack
    |> getParameters 1
    |> Result.bind (fun (parameters, remainingStack) ->
        printValue parameters[0]
        Ok remainingStack)
    
let NativeSwap (stack: Value list) =
    stack
    |> getParameters 2
    |> Result.bind (fun (parameters, remainingStack) ->
        Ok (parameters[1]::parameters[0]::remainingStack))

let NativeWords = [
    { Symbol = "+"; Instructions = Native NativeAdd }
    { Symbol = "-"; Instructions = Native NativeSubtract }
    { Symbol = "*"; Instructions = Native NativeMultiply }
    { Symbol = "/"; Instructions = Native NativeDivide }
    { Symbol = "%"; Instructions = Native NativeModulus }
    { Symbol = "dup"; Instructions = Native NativeDup }
    { Symbol = "drop"; Instructions = Native NativeDrop }
    { Symbol = "."; Instructions = Native NativeDrop }
    { Symbol = "swap"; Instructions = Native NativeSwap }
]
