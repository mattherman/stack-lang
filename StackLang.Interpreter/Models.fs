module StackLang.Interpreter.Models

type Interpreter =
    { Dictionary: Map<string, Word>
      Stack: Value list }

and Value =
    | Integer of int
    | Float of double
    | String of string
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
    | Array a ->
        let getTokens = Array.map valueToString >> String.concat " "
        $"{{ {getTokens a} }}"
    | Quotation q ->
        let getTokens = List.map valueToString >> String.concat " "
        $"[ {getTokens q} ]"
    | Word w -> $"%s{w}"

let rec printValue value = printfn $"%s{valueToString value}"
