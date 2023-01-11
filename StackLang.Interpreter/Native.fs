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

let operationWithThreeParameters (operation: Value * Value * Value * Value list -> Result<Value list, string>) (stack: Value list) =
    stack
    |> operationWithParameters 3 (fun (parameters, remainingStack) ->
        operation (parameters[0], parameters[1], parameters[2], remainingStack))

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

let NativeFilter (dictionary: Map<string, Word>) (stack: Value list) =
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
                    |> Result.map List.head
                    |> Result.map (fun boolean -> (value, boolean)))
                |> List.collectResults
                |> Result.map (fun values ->
                    values
                    |> List.filter (fun (_, result) ->
                        match result with
                        | Boolean b -> b
                        | _ -> false)
                    |> List.map fst)
                |> Result.map (List.toArray >> Array)
                |> Result.map (fun mappedArray -> mappedArray :: remainingStack)
            | _ -> Error "Filter expected an array"
        | _ -> Error "Filter expected a quotation")

let booleanOperation op =
    operationWithTwoParameters (fun (left, right, remainingStack) ->
        let result = (op right left) |> Boolean
        result :: remainingStack |> Ok)

let NativeEquals (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (=) stack

let NativeGreaterThan (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (>) stack

let NativeLessThan (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (<) stack

let NativeGreaterThanOrEqual (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (>=) stack

let NativeLessThanOrEqual (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (<=) stack

let NativeNot (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (boolean, remainingStack) ->
        match boolean with
        | Boolean value ->
            let result = not value |> Boolean
            result :: remainingStack |> Ok
        | _ -> Error "Not expected a boolean")

let NativeIf (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (falseQuotation, trueQuotation, boolean, remainingStack) ->
        match boolean with
        | Boolean b ->
            let quotation = if b then trueQuotation else falseQuotation
            match quotation with
            | Quotation q ->
                executeInstructions (Compiled q) dictionary remainingStack
            | _ -> Error "Expected a quotation"
        | _ -> Error "Expected a boolean")

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
    { Symbol = "filter"; Instructions = Native NativeFilter }
    { Symbol = "="; Instructions = Native NativeEquals }
    { Symbol = ">"; Instructions = Native NativeGreaterThan }
    { Symbol = "<"; Instructions = Native NativeLessThan }
    { Symbol = ">="; Instructions = Native NativeGreaterThanOrEqual }
    { Symbol = "<="; Instructions = Native NativeLessThanOrEqual }
    { Symbol = "not"; Instructions = Native NativeNot }
    { Symbol = "if"; Instructions = Native NativeIf }
]
