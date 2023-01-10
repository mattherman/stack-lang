module Tests

open Xunit
open FsUnit.Xunit
open StackLang.Interpreter
open StackLang.Interpreter.Models
open StackLang.Interpreter.Native

let interpretMultiple (input: string list) =
    let interpreter = Interpreter.createInterpreter ()

    input
    |> List.fold
        (fun previousResult next ->
            match previousResult with
            | Ok i -> Interpreter.run next i
            | Error msg -> Error msg)
        (Ok interpreter)

let interpret (input: string) = interpretMultiple [ input ]

let assertStackMatches (expectedStack: Value list) (result: Result<Interpreter, string>) =
    match result with
    | Ok result -> result.Stack |> should equal expectedStack
    | Error msg -> Assert.True(false, msg)

let assertError (expectedMessage: string) (result: Result<'a, string>) =
    match result with
    | Ok result -> Assert.True(false, $"Expected error but got {result}")
    | Error msg -> msg |> should equal expectedMessage

[<Fact>]
let ``Can push primitive values onto the stack`` () =
    interpret "5 8.3 \"string\""
    |> assertStackMatches [ String "string"; Float 8.3; Integer 5 ]

[<Fact>]
let ``Can push arrays onto the stack`` () =
    interpret "{ 1 2 3 }"
    |> assertStackMatches [ Array [| Integer 1; Integer 2; Integer 3 |] ]

[<Fact>]
let ``Can push quotations onto the stack`` () =
    interpret "[ 1 + ]"
    |> assertStackMatches [ Quotation [ Integer 1; Word "+" ] ]

// [<Fact>]
// let ``Can nest quotations`` () =
//     interpret "[ 1 + [ 2 - ] ]"
//     |> assertStackMatches [ Quotation [ "1"; "+"; "["; "2"; "-"; "]" ] ]

[<Fact>]
let ``Addition requires two elements`` () =
    interpret "1 +" |> assertError "Stack underflow"

[<Fact>]
let ``Can add integers`` () =
    interpret "6 2 +" |> assertStackMatches [ Integer 8 ]

[<Fact>]
let ``Can add floats`` () =
    interpret "6.5 3.1 +" |> assertStackMatches [ Float 9.6 ]

[<Fact>]
let ``Can add strings`` () =
    interpret "\"a\" \"r\" \"ray\" + +" |> assertStackMatches [ String "array" ]

[<Fact>]
let ``Subtraction requires two elements`` () =
    interpret "1 -" |> assertError "Stack underflow"

[<Fact>]
let ``Can subtract integers`` () =
    interpret "4 3 -" |> assertStackMatches [ Integer 1 ]

[<Fact>]
let ``Can subtract floats`` () =
    let result = interpret "5.2 3.8 -"

    match result with
    | Ok result ->
        result.Stack |> should haveLength 1
        result.Stack |> List.head |> should (equalWithin 0.1) |> ignore
    | Error msg -> Assert.True(false, msg)

[<Fact>]
let ``Multiplication requires two elements`` () =
    interpret "1 *" |> assertError "Stack underflow"

[<Fact>]
let ``Can multiply integers`` () =
    interpret "3 2 *" |> assertStackMatches [ Integer 6 ]

[<Fact>]
let ``Can multiply floats`` () =
    interpret "3.3 4.2 *" |> assertStackMatches [ Float 13.86 ]

[<Fact>]
let ``Division requires two elements`` () =
    interpret "1 /" |> assertError "Stack underflow"

[<Fact>]
let ``Can divide integers`` () =
    interpret "8 4 /" |> assertStackMatches [ Integer 2 ]

[<Fact>]
let ``Can divide floats`` () =
    interpret "12.5 2.5 /" |> assertStackMatches [ Float 5.0 ]

[<Fact>]
let ``Modulus requires two elements`` () =
    interpret "1 %" |> assertError "Stack underflow"

[<Fact>]
let ``Can calculate modulus of integers`` () =
    interpret "8 3 %" |> assertStackMatches [ Integer 2 ]

[<Fact>]
let ``Can calculate modulus of floats`` () =
    let result = interpret "3.5 2.2 %"

    match result with
    | Ok result ->
        result.Stack |> should haveLength 1
        result.Stack |> List.head |> should (equalWithin 0.1) |> ignore
    | Error msg -> Assert.True(false, msg)

[<Fact>]
let ``Cannot duplicate an empty stack`` () =
    interpret "dup" |> assertError "Stack underflow"

[<Fact>]
let ``Can duplicate stack elements`` () =
    interpret "3 dup" |> assertStackMatches [ Integer 3; Integer 3 ]

[<Fact>]
let ``Swap requires two elements`` () =
    interpret "1 swap" |> assertError "Stack underflow"

[<Fact>]
let ``Can swap stack elements`` () =
    interpret "3 2 swap" |> assertStackMatches [ Integer 3; Integer 2 ]

[<Fact>]
let ``Cannot drop an empty stack`` () =
    interpret "drop" |> assertError "Stack underflow"

[<Fact>]
let ``Can drop stack elements with 'drop'`` () =
    interpret "3 2 drop" |> assertStackMatches [ Integer 3 ]

[<Fact>]
let ``Can drop stack elements with '.'`` () =
    interpret "3 2 ." |> assertStackMatches [ Integer 3 ]

[<Fact>]
let ``Can clear the stack`` () =
    interpret "1 2 3 4 5 clear" |> assertStackMatches []

[<Fact>]
let ``Can define and execute custom words`` () =
    interpretMultiple [ ": square dup * ;"; "5 square" ]
    |> assertStackMatches [ Integer 25 ]
