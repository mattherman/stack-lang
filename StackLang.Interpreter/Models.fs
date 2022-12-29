module StackLang.Interpreter.Models

type Interpreter = {
    Dictionary: Map<string, Word>;
    Stack: Value list
}
and Value =
    | Literal of int
    | Word of Word
and Instructions =
    | Native of (Value list -> Result<Value list, string>)
    | Compiled of string list
and Word = {
    Symbol: string
    Instructions: Instructions
}

let printValue value =
    match value with
    | Literal l -> printfn $"%d{l}"
    | Word w -> printfn $"%s{w.Symbol}"
