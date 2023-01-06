module StackLang.Interpreter.Models

type Interpreter =
    { Dictionary: Map<string, Word>
      Stack: Value list }

and Value =
    | Integer of int
    | Float of double
    | String of string
    | Array of Value[]
    | Quotation of string list

and Instructions =
    | Native of (Value list -> Result<Value list, string>)
    | Compiled of string list

and Word =
    { Symbol: string
      Instructions: Instructions }

let rec valueToString value =
    match value with
    | Integer i -> $"%d{i}"
    | Float l -> $"%f{l}"
    | String s -> $"\"%s{s}\""
    | Array a ->
        let arrayValues = String.concat " " (Array.map valueToString a)
        $"{{ {arrayValues} }}"
    | Quotation q ->
        let quotationTokens = String.concat " " q
        $"[ {quotationTokens} ]"

let rec printValue value = printfn $"%s{valueToString value}"
