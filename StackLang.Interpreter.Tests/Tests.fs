module Tests

open Xunit
open FsUnit.Xunit
open StackLang.Interpreter
open StackLang.Interpreter.Models

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
    interpret "5 8.3 \"string\" t"
    |> assertStackMatches [ Boolean true; String "string"; Float 8.3; Integer 5 ]

[<Fact>]
let ``Can push strings with spaces onto the stack`` () =
    interpret "\"string with spaces\""
    |> assertStackMatches [ String "string with spaces" ]

[<Fact>]
let ``Can escape quotes in strings`` () =
    // Input:    "some \"quotes\" in this"
    // Expected: "some "quotes" in this"
    interpret "\"some \\\"quotes\\\" in this\""
    |> assertStackMatches [ String "some \"quotes\" in this" ]

[<Fact>]
let ``Can escape an escape character in a string`` () =
    // Input:    "escape \\z this"
    // Expected: "escape \z this"
    interpret "\"escape \\\\z this\""
    |> assertStackMatches [ String "escape \z this" ]

[<Fact>]
let ``Can push arrays onto the stack`` () =
    interpret "{ 1 2 3 }"
    |> assertStackMatches [ Array [| Integer 1; Integer 2; Integer 3 |] ]

[<Fact>]
let ``Can nest arrays`` () =
    interpret "{ { 1 2 } { 3 } }"
    |> assertStackMatches [
        Array [|
            Array [| Integer 1; Integer 2 |]
            Array [| Integer 3 |]
        |]
    ]

[<Fact>]
let ``Can push quotations onto the stack`` () =
    interpret "[ 1 + ]"
    |> assertStackMatches [ Quotation [ Integer 1; Word "+" ] ]

[<Fact>]
let ``Can nest quotations`` () =
    interpret "[ 1 + [ 2 - ] ]"
    |> assertStackMatches [
        Quotation [
            Integer 1
            Word "+"
            Quotation [
                Integer 2
                Word "-"
            ]
        ]
    ]

[<Fact>]
let ``Can nest quotations and arrays arbitrarily deep`` () =
    interpret "[ { { 1 } [ * dup ] } [ { 2 } { 3 } swap ] ]"
    |> assertStackMatches [
        Quotation [
            Array [|
                Array [| Integer 1 |]
                Quotation [
                    Word "*"
                    Word "dup"
                ]
            |]
            Quotation [
                Array [| Integer 2 |]
                Array [| Integer 3 |]
                Word "swap"
            ]
        ]
    ]

[<Fact>]
let ``Cannot nest word definitions`` () =
    interpret ": outer : inner 1 + ; ;" |> assertError "Unable to parse literal: :"

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

[<Fact>]
let ``Can evaluate quotations`` () =
    interpret "1 [ 2 + ] eval" |> assertStackMatches [ Integer 3 ]

[<Fact>]
let ``Can map arrays`` () =
    interpret "{ 1 2 3 } [ 1 + ] map"
    |> assertStackMatches [ Array [| Integer 2; Integer 3; Integer 4 |] ]

[<Fact>]
let ``Can filter arrays`` () =
    interpret "{ 1 2 3 4 } [ 2 % 0 = ] filter"
    |> assertStackMatches [ Array [| Integer 2; Integer 4 |] ]

[<Fact>]
let ``Can compare two values for equality`` () =
    interpret "1 1 =" |> assertStackMatches [ Boolean true ]
    interpret "1 2 =" |> assertStackMatches [ Boolean false ]

[<Fact>]
let ``Can compare whether one value is greater than another`` () =
    interpret "2 1 >" |> assertStackMatches [ Boolean true ]
    interpret "1 2 >" |> assertStackMatches [ Boolean false ]

[<Fact>]
let ``Can compare whether one value is less than another`` () =
    interpret "1 2 <" |> assertStackMatches [ Boolean true ]
    interpret "2 1 <" |> assertStackMatches [ Boolean false ]

[<Fact>]
let ``Can compare whether one value is greater than or equal to another`` () =
    interpret "2 1 >=" |> assertStackMatches [ Boolean true ]
    interpret "1 2 >=" |> assertStackMatches [ Boolean false ]
    interpret "2 2 >=" |> assertStackMatches [ Boolean true ]

[<Fact>]
let ``Can compare whether one value is less than or equal to another`` () =
    interpret "1 2 <=" |> assertStackMatches [ Boolean true ]
    interpret "2 1 <=" |> assertStackMatches [ Boolean false ]
    interpret "2 2 <=" |> assertStackMatches [ Boolean true ]

[<Fact>]
let ``Can negate values`` () =
      interpret "t not" |> assertStackMatches [ Boolean false ]
      interpret "f not" |> assertStackMatches [ Boolean true ]

[<Fact>]
let ``Can perform conditional branching`` () =
    interpret "t [ 1 ] [ 2 ] if" |> assertStackMatches [ Integer 1 ]
    interpret "f [ 1 ] [ 2 ] if" |> assertStackMatches [ Integer 2 ]
