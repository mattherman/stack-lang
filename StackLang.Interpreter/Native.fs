module StackLang.Interpreter.Native

open Models

let getParameters count (stack: Value list) =
    if stack.Length >= count then
        Ok (List.take count stack, List.skip count stack)
    else
        Error "Stack underflow"
        
let operationWithParameters parameterCount operation (stack: Value list) =
    stack
    |> getParameters parameterCount
    |> Result.bind operation
    
let operationWithOneParameter (operation: Value * Value list -> Result<Value list, string>) (stack: Value list) =
    stack
    |> operationWithParameters 1 (fun (parameters, remainingStack) ->
        operation (parameters[0], remainingStack))

let operationWithTwoParameters (operation: Value * Value * Value list -> Result<Value list, string>) (stack: Value list) =
    stack
    |> operationWithParameters 2 (fun (parameters, remainingStack) ->
        operation (parameters[0], parameters[1], remainingStack))

let NativeAdd (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt + firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat + firstFloat))
        | String firstStr, String secondStr -> Ok (String (secondStr + firstStr))
        | _ -> Error "Values do not support operator (+)"
        |> Result.map (fun value -> value::remainingStack))

let NativeSubtract (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt - firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat - firstFloat))
        | _ -> Error "Values do not support operator (-)"
        |> Result.map (fun value -> value::remainingStack))

let NativeMultiply (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt * firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat * firstFloat))
        | _ -> Error "Values do not support operator (*)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDivide (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt / firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat / firstFloat))
        | _ -> Error "Values do not support operator (/)"
        |> Result.map (fun value -> value::remainingStack))

let NativeModulus (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt % firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat % firstFloat))
        | _ -> Error "Values do not support operator (%)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDup (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        Ok (token::token::remainingStack))
    
let NativeDrop (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        printValue token
        Ok remainingStack)
    
let NativeSwap (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        Ok (second::first::remainingStack))
    
let NativeClear (_: Value list) =
    Ok []
    


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
    { Symbol = "clear"; Instructions = Native NativeClear }
]
