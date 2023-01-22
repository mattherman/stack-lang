namespace StackLang.Interpreter

open StackLang.Interpreter.Models

module ExecutionEngine =

    type Engine() =

        abstract member Execute: Value * Map<string, Word> * Value list -> Result<Value list, string>
        default this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
            (this :> IExecutionEngine).Execute (value, dictionary, stack)

        abstract member ExecuteInstructions: Instructions * Map<string, Word> * Value list -> Result<Value list, string>
        default this.ExecuteInstructions (instructions, dictionary, stack) =
            (this :> IExecutionEngine).ExecuteInstructions (instructions, dictionary, stack)

        interface IExecutionEngine with
            member this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
                match value with
                | Word wordSymbol ->
                    match dictionary.TryGetValue(wordSymbol) with
                    | true, word -> this.ExecuteInstructions (word.Instructions, dictionary, stack)
                    | _ -> Error $"No word named {wordSymbol} found in current vocabulary"
                | _ -> value :: stack |> Ok

            member this.ExecuteInstructions (instructions, dictionary, stack) =
                match instructions with
                | Native nativeWord ->
                    nativeWord dictionary stack
                | Compiled compiledWord ->
                    compiledWord
                    |> List.fold (fun newStack nextToken ->
                        newStack
                        |> Result.bind (fun newStack ->
                            this.Execute (nextToken, dictionary, newStack)))
                        (Ok stack)

    type DebugEngine() =

        inherit Engine()

        override this.Execute (value: Value, dictionary: Map<string, Word>, stack: Value list) =
            printfn "In DebugEngine.Execute"
            base.Execute (value, dictionary, stack)

        override this.ExecuteInstructions (instructions, dictionary, stack) =
            printfn "In DebugEngine.ExecuteInstructions"
            base.ExecuteInstructions (instructions, dictionary, stack)