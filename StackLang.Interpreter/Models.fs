module StackLang.Interpreter.Models

type Interpreter = {
    Dictionary: Map<string, Word>;
    Stack: Value list
}
and Value =
    | Integer of int
    | Float of double
    | String of string
and Instructions =
    | Native of (Value list -> Result<Value list, string>)
    | Compiled of string list
and Word = {
    Symbol: string
    Instructions: Instructions
}

let printValue value =
    match value with
    | Integer i -> printfn $"%d{i}"
    | Float l -> printfn $"%f{l}"
    | String s -> printfn $"\"%s{s}\""
