module ParserCombinators.Test

open NUnit.Framework
open FsUnit

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

let parseChar char (stream: string)  =
    if stream = "" then
        Failure "No more input"
    else
        let first = stream.[0]
        let rest = stream.[1..]

        if first = char then
            Success(char, rest)
        else
            Failure $"Expecting '{char}'. Got '{first}'"


[<Test>]
let empty_string () =
    let expected: ParseResult<char * string> = Failure "No more input"
    "" |> parseChar 'A' |> should equal expected

[<Test>]
let when_char_is_found_return_message_and_rest_of_the_stream () =
    let expected = Success(('A', "rest"))
    "Arest" |> parseChar 'A' |> should equal expected

[<Test>]
let when_char_is_not_found_return_false_and_unmodified_stream () =
    let expected: ParseResult<char * string> = Failure "Expecting 'A'. Got 'r'"
    "rest" |> parseChar 'A' |> should equal expected

[<Test>]
let only_char () =
    let expected = Success('A', "")
    "A" |> parseChar 'A' |> should equal expected
