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

let NativeAdd (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt + firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat + firstFloat))
        | String firstStr, String secondStr -> Ok (String (secondStr + firstStr))
        | _ -> Error "Values do not support operator (+)"
        |> Result.map (fun value -> value::remainingStack))

let NativeSubtract (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt - firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat - firstFloat))
        | _ -> Error "Values do not support operator (-)"
        |> Result.map (fun value -> value::remainingStack))

let NativeMultiply (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt * firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat * firstFloat))
        | _ -> Error "Values do not support operator (*)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDivide (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt / firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat / firstFloat))
        | _ -> Error "Values do not support operator (/)"
        |> Result.map (fun value -> value::remainingStack))

let NativeModulus (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt % firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat % firstFloat))
        | _ -> Error "Values do not support operator (%)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDup (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        Ok (token::token::remainingStack))
    
let NativeDrop (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        Ok remainingStack)

let NativePrintAndDrop (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        printValue token
        Ok remainingStack)
    
let NativeSwap (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        Ok (second::first::remainingStack))
    
let NativeClear (engine: IExecutionEngine) (_: Map<string, Word>) (_: Value list) =
    Ok []

let NativeCall (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (quotation, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            engine.ExecuteInstructions ((Compiled instructions), dictionary, remainingStack)
        | _ -> Error "Value does not support eval")

let NativeMap (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation, array with
        | Quotation instructions, Array arr ->
            arr
            |> Array.toList
            |> List.map (fun value ->
                engine.ExecuteInstructions (Compiled instructions, dictionary, [ value ])
                |> Result.map List.head)
            |> List.collectResults
            |> Result.map (List.toArray >> Array)
            |> Result.map (fun mappedArray -> mappedArray :: remainingStack)
        | _ -> Error "Map expected a quotation and an array")

let NativeFilter (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation, array with
        | Quotation instructions, Array arr ->
            arr
            |> Array.toList
            |> List.map (fun value ->
                engine.ExecuteInstructions (Compiled instructions, dictionary, [ value ])
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

let NativeEquals (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (=) stack

let NativeGreaterThan (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (>) stack

let NativeLessThan (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (<) stack

let NativeNot (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (boolean, remainingStack) ->
        match boolean with
        | Boolean value ->
            let result = not value |> Boolean
            result :: remainingStack |> Ok
        | _ -> Error "Not expected a boolean")

let NativeIf (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (falseQuotation, trueQuotation, boolean, remainingStack) ->
        match boolean with
        | Boolean b ->
            let quotation = if b then trueQuotation else falseQuotation
            match quotation with
            | Quotation q ->
                engine.ExecuteInstructions ((Compiled q), dictionary, remainingStack)
            | _ -> Error "Expected a quotation"
        | _ -> Error "Expected a boolean")

let NativeDip (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            engine.ExecuteInstructions ((Compiled instructions), dictionary, remainingStack)
            |> Result.map (fun resultStack ->
                value :: resultStack)
        | _ -> Error "Expected a quotation")

let NativeOver (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        second :: first :: second :: remainingStack |> Ok)

let NativeCurry (engine: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            let newQuotation = Quotation (value :: instructions)
            newQuotation :: remainingStack |> Ok
        | _ -> Error "Expected a quotation")

let NativeWords (engine: IExecutionEngine) = [
    { Symbol = "+"; Instructions = Native (NativeAdd engine) }
    { Symbol = "-"; Instructions = Native (NativeSubtract engine) }
    { Symbol = "*"; Instructions = Native (NativeMultiply engine) }
    { Symbol = "/"; Instructions = Native (NativeDivide engine) }
    { Symbol = "%"; Instructions = Native (NativeModulus engine) }
    { Symbol = "dup"; Instructions = Native (NativeDup engine) }
    { Symbol = "drop"; Instructions = Native (NativeDrop engine) }
    { Symbol = "."; Instructions = Native (NativePrintAndDrop engine) }
    { Symbol = "swap"; Instructions = Native (NativeSwap engine) }
    { Symbol = "clear"; Instructions = Native (NativeClear engine) }
    { Symbol = "print"; Instructions = Native (NativePrintAndDrop engine) }
    { Symbol = "call"; Instructions = Native (NativeCall engine) }
    { Symbol = "map"; Instructions = Native (NativeMap engine) }
    { Symbol = "filter"; Instructions = Native (NativeFilter engine) }
    { Symbol = "="; Instructions = Native (NativeEquals engine) }
    { Symbol = ">"; Instructions = Native (NativeGreaterThan engine) }
    { Symbol = "<"; Instructions = Native (NativeLessThan engine) }
    { Symbol = "not"; Instructions = Native (NativeNot engine) }
    { Symbol = "if"; Instructions = Native (NativeIf engine) }
    { Symbol = "dip"; Instructions = Native (NativeDip engine) }
    { Symbol = "over"; Instructions = Native (NativeOver engine) }
    { Symbol = "curry"; Instructions = Native (NativeCurry engine) }
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