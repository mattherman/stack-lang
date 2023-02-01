module StackLang.Interpreter.Models

type IExecutionEngine =
    abstract Execute: value: Value * dictionary: Map<string, Word> * stack: Value list -> Result<Value list, string>
    abstract ExecuteInstructions: source: Value * instructions: Instructions * dictionary: Map<string, Word> * stack: Value list -> Result<Value list, string>
    abstract AttachDebugger: Debugger -> unit
    abstract DetachDebugger: unit -> unit
    abstract IsDebuggerAttached: unit -> bool
    abstract SetStep: bool -> unit

and IFrame =
    abstract Advance: unit -> unit
    abstract InstructionIndex: int
    abstract Instructions: Value list
    abstract CurrentInstruction: Value
    abstract RemainingInstructions: unit -> Value list
    abstract Source: Value

and DebuggerCommand =
    | StepNext
    | StepInto
    | StepOut
    | Continue

and Debugger = {
    OnExecute: Value * IFrame option * Value list -> DebuggerCommand
    OnError: string * IFrame list * Value list -> DebuggerCommand
}

and Interpreter =
    { Dictionary: Map<string, Word>
      Stack: Value list
      Engine: IExecutionEngine }

and Value =
    | Integer of int
    | Float of double
    | String of string
    | Boolean of bool
    | Array of Value[]
    | Quotation of Value list
    | Word of string

and Instructions =
    | Native of (Map<string, Word> -> Value list -> Result<Value list, string>)
    | Compiled of Value list

and Word =
    { Symbol: string
      Instructions: Instructions }

let rec valueToString value =
    match value with
    | Integer i -> $"%d{i}"
    | Float l -> $"%f{l}"
    | String s -> $"\"%s{s}\""
    | Boolean b ->
        let value = if b then "t" else "f"
        $"%s{value}"
    | Array a ->
        let getTokens = Array.map valueToString >> String.concat " "
        $"{{ {getTokens a} }}"
    | Quotation q ->
        let getTokens = List.map valueToString >> String.concat " "
        $"[ {getTokens q} ]"
    | Word w -> $"%s{w}"

let rec printValue value = printfn $"%s{valueToString value}"

module List =
    let collectResults (list: Result<'a, 'e> list) : Result<'a list, 'e> =
        List.foldBack
            (fun next acc ->
                match acc with
                | Ok results ->
                    match next with
                    | Ok nextResult -> Ok(nextResult :: results)
                    | Error msg -> Error msg
                | Error msg -> Error msg)
            list
            (Ok [])