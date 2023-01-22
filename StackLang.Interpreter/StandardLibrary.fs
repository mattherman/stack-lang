module StackLang.Interpreter.StandardLibrary

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
        Ok remainingStack)

let NativePrintAndDrop (_: Map<string, Word>) (stack: Value list) =
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

let NativeCall (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (quotation, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            executeInstructions (Compiled instructions) dictionary remainingStack
        | _ -> Error "Value does not support eval")

let NativeMap (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation, array with
        | Quotation instructions, Array arr ->
            arr
            |> Array.toList
            |> List.map (fun value ->
                executeInstructions (Compiled instructions) dictionary [ value ]
                |> Result.map List.head)
            |> List.collectResults
            |> Result.map (List.toArray >> Array)
            |> Result.map (fun mappedArray -> mappedArray :: remainingStack)
        | _ -> Error "Map expected a quotation and an array")

let NativeFilter (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation, array with
        | Quotation instructions, Array arr ->
            arr
            |> Array.toList
            |> List.map (fun value ->
                executeInstructions (Compiled instructions) dictionary [ value ]
                |> Result.map (fun resultStack ->
                    match List.head resultStack with
                    | Boolean b ->
                        if b then Some value else None
                    | _ -> None))
            |> List.collectResults
            |> Result.map (List.choose id >> List.toArray >> Array)
            |> Result.map (fun mappedArray -> mappedArray :: remainingStack)
        | _ -> Error "Filter expected a quotation and an array")

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

let NativeDip (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            executeInstructions (Compiled instructions) dictionary remainingStack
            |> Result.map (fun resultStack ->
                value :: resultStack)
        | _ -> Error "Expected a quotation")

let NativeOver (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        second :: first :: second :: remainingStack |> Ok)

let NativeCurry (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            let newQuotation = Quotation (value :: instructions)
            newQuotation :: remainingStack |> Ok
        | _ -> Error "Expected a quotation")

let NativeWords = [
    { Symbol = "+"; Instructions = Native NativeAdd }
    { Symbol = "-"; Instructions = Native NativeSubtract }
    { Symbol = "*"; Instructions = Native NativeMultiply }
    { Symbol = "/"; Instructions = Native NativeDivide }
    { Symbol = "%"; Instructions = Native NativeModulus }
    { Symbol = "dup"; Instructions = Native NativeDup }
    { Symbol = "drop"; Instructions = Native NativeDrop }
    { Symbol = "."; Instructions = Native NativePrintAndDrop }
    { Symbol = "swap"; Instructions = Native NativeSwap }
    { Symbol = "clear"; Instructions = Native NativeClear }
    { Symbol = "print"; Instructions = Native NativePrintAndDrop }
    { Symbol = "call"; Instructions = Native NativeCall }
    { Symbol = "map"; Instructions = Native NativeMap }
    { Symbol = "filter"; Instructions = Native NativeFilter }
    { Symbol = "="; Instructions = Native NativeEquals }
    { Symbol = ">"; Instructions = Native NativeGreaterThan }
    { Symbol = "<"; Instructions = Native NativeLessThan }
    { Symbol = "not"; Instructions = Native NativeNot }
    { Symbol = "if"; Instructions = Native NativeIf }
    { Symbol = "dip"; Instructions = Native NativeDip }
    { Symbol = "over"; Instructions = Native NativeOver }
    { Symbol = "curry"; Instructions = Native NativeCurry }
]

let CompiledWords = @"

: <= > not ;
: >= < not ;
: keep over [ call ] dip ;
: bi [ keep ] dip call ;
: bi* [ dip ] dip call ;
: bi@ dup bi* ;
: tri [ [ keep ] dip keep ] dip call ;
: 2curry curry curry ;
: unless swap [ drop ] [ call ] if ;

"