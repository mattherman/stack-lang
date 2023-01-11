module StackLang.Interpreter.Native

open Models
open ExecutionEngine

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

let NativeAdd (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt + firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat + firstFloat))
        | String firstStr, String secondStr -> Ok (String (secondStr + firstStr))
        | _ -> Error "Values do not support operator (+)"
        |> Result.map (fun value -> value::remainingStack))

let NativeSubtract (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt - firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat - firstFloat))
        | _ -> Error "Values do not support operator (-)"
        |> Result.map (fun value -> value::remainingStack))

let NativeMultiply (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt * firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat * firstFloat))
        | _ -> Error "Values do not support operator (*)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDivide (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt / firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat / firstFloat))
        | _ -> Error "Values do not support operator (/)"
        |> Result.map (fun value -> value::remainingStack))

let NativeModulus (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt % firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat % firstFloat))
        | _ -> Error "Values do not support operator (%)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDup (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        Ok (token::token::remainingStack))
    
let NativeDrop (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        printValue token
        Ok remainingStack)
    
let NativeSwap (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        Ok (second::first::remainingStack))
    
let NativeClear (_: Map<string, Word>) (_: Value list) =
    Ok []
    
let NativeEval (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (quotation, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            executeInstructions (Compiled instructions) dictionary remainingStack
        | _ -> Error "Value does not support eval")

let NativeMap (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            match array with
            | Array arr ->
                arr
                |> Array.toList
                |> List.map (fun value ->
                    executeInstructions (Compiled instructions) dictionary [ value ]
                    |> Result.map List.head)
                |> List.collectResults
                |> Result.map (List.toArray >> Array)
                |> Result.map (fun mappedArray -> mappedArray :: remainingStack)
            | _ -> Error "Map expected an array"
        | _ -> Error "Map expected a quotation")

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
    { Symbol = "eval"; Instructions = Native NativeEval }
    { Symbol = "map"; Instructions = Native NativeMap }
]
