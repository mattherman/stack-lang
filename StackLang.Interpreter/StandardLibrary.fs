module StackLang.Interpreter.StandardLibrary

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

let operationWithThreeParameters (operation: Value * Value * Value * Value list -> Result<Value list, string>) (stack: Value list) =
    stack
    |> operationWithParameters 3 (fun (parameters, remainingStack) ->
        operation (parameters[0], parameters[1], parameters[2], remainingStack))

let operationWithFourParameters (operation: Value * Value * Value * Value * Value list -> Result<Value list, string>) (stack: Value list) =
    stack
    |> operationWithParameters 4 (fun (parameters, remainingStack) ->
        operation (parameters[0], parameters[1], parameters[2], parameters[3], remainingStack))

let NativeAdd (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt + firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat + firstFloat))
        | String firstStr, String secondStr -> Ok (String (secondStr + firstStr))
        | _ -> Error "Values do not support operator (+)"
        |> Result.map (fun value -> value::remainingStack))

let NativeSubtract (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt - firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat - firstFloat))
        | _ -> Error "Values do not support operator (-)"
        |> Result.map (fun value -> value::remainingStack))

let NativeMultiply (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt * firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat * firstFloat))
        | _ -> Error "Values do not support operator (*)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDivide (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt / firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat / firstFloat))
        | _ -> Error "Values do not support operator (/)"
        |> Result.map (fun value -> value::remainingStack))

let NativeModulus (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (left, right, remainingStack) ->
        match left, right with
        | Integer firstInt, Integer secondInt -> Ok (Integer (secondInt % firstInt))
        | Float firstFloat, Float secondFloat -> Ok (Float (secondFloat % firstFloat))
        | _ -> Error "Values do not support operator (%)"
        |> Result.map (fun value -> value::remainingStack))

let NativeDup (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        Ok (token::token::remainingStack))

let NativeTwoDup (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        first :: second :: first :: second :: remainingStack |> Ok)

let NativeThreeDup (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (first, second, third, remainingStack) ->
        first :: second :: third :: first :: second :: third :: remainingStack |> Ok)

let NativeFourDup (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithFourParameters (fun (first, second, third, fourth, remainingStack) ->
        first :: second :: third :: fourth :: first :: second :: third :: fourth :: remainingStack |> Ok)

let NativeDrop (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (_, remainingStack) ->
        Ok remainingStack)

let NativePrintAndDrop (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (token, remainingStack) ->
        printValue token
        Ok remainingStack)
    
let NativeSwap (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        Ok (second::first::remainingStack))
    
let NativeClear (_: IExecutionEngine) (_: Map<string, Word>) (_: Value list) =
    Ok []

let NativeCall (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (quotation, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, remainingStack)
        | _ -> Error "Value does not support eval")

let NativeMap (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, array, remainingStack) ->
        match quotation, array with
        | Quotation instructions, Array arr ->
            arr
            |> Array.toList
            |> List.map (fun value ->
                engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, [ value ])
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
                engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, [ value ])
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

let NativeEquals (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (=) stack

let NativeGreaterThan (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (>) stack

let NativeLessThan (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    booleanOperation (<) stack

let NativeNot (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
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
            | Quotation instructions ->
                engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, remainingStack)
            | _ -> Error "Expected a quotation"
        | _ -> Error "Expected a boolean")

let NativeDip (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, remainingStack)
            |> Result.map (fun resultStack ->
                value :: resultStack)
        | _ -> Error "Expected a quotation")

let NativeOver (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, second, remainingStack) ->
        second :: first :: second :: remainingStack |> Ok)

let NativeCurry (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (quotation, value, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            let newQuotation = Quotation (value :: instructions)
            newQuotation :: remainingStack |> Ok
        | _ -> Error "Expected a quotation")

let NativeTime (engine: IExecutionEngine) (dictionary: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithOneParameter (fun (quotation, remainingStack) ->
        match quotation with
        | Quotation instructions ->
            let stopwatch = System.Diagnostics.Stopwatch.StartNew()
            engine.ExecuteInstructions (quotation, Compiled instructions, dictionary, remainingStack)
            |> Result.map (fun resultStack ->
                stopwatch.Stop()
                // Warning: Potentially truncates values
                let elapsed = int32(stopwatch.ElapsedMilliseconds)
                (Integer elapsed) :: resultStack)
        | _ -> Error "Expected a quotation")

let NativeRot (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (first, second, third, remainingStack) ->
        third :: first :: second :: remainingStack |> Ok)

let NativeMinusRot (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (first, second, third, remainingStack) ->
        second :: third :: first :: remainingStack |> Ok)

let NativeNip (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithTwoParameters (fun (first, _, remainingStack) ->
        first :: remainingStack |> Ok)

let NativePick (_: IExecutionEngine) (_: Map<string, Word>) (stack: Value list) =
    stack
    |> operationWithThreeParameters (fun (first, second, third, remainingStack) ->
        third :: first :: second :: third :: remainingStack |> Ok)

let NativeWords (engine: IExecutionEngine) = [
    { Symbol = "+"; Instructions = Native (NativeAdd engine) }
    { Symbol = "-"; Instructions = Native (NativeSubtract engine) }
    { Symbol = "*"; Instructions = Native (NativeMultiply engine) }
    { Symbol = "/"; Instructions = Native (NativeDivide engine) }
    { Symbol = "%"; Instructions = Native (NativeModulus engine) }
    { Symbol = "dup"; Instructions = Native (NativeDup engine) }
    { Symbol = "2dup"; Instructions = Native (NativeTwoDup engine) }
    { Symbol = "3dup"; Instructions = Native (NativeThreeDup engine) }
    { Symbol = "4dup"; Instructions = Native (NativeFourDup engine) }
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
    { Symbol = "time"; Instructions = Native (NativeTime engine) }
    { Symbol = "rot"; Instructions = Native (NativeRot engine) }
    { Symbol = "-rot"; Instructions = Native (NativeMinusRot engine) }
    { Symbol = "nip"; Instructions = Native (NativeNip engine) }
    { Symbol = "pick"; Instructions = Native (NativePick engine) }
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
: 3curry curry curry curry ;
: unless swap [ drop ] [ call ] if ;
: 2drop drop drop ;
: 3drop drop drop drop ;
: 4drop drop drop drop drop ;

"