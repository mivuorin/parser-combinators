module ParserCombinators.Test

open NUnit.Framework
open FsUnit
open Parser
open ParserCombinators.Parser

let parseChar char =
    let parser stream = 
        if stream = "" then
            Failure "No more input"
        else
            let first = stream.[0]
            let rest = stream.[1..]

            if first = char then
                Success(char, rest)
            else
                Failure $"Expecting '{char}'. Got '{first}'"
    Parser parser

let parseA: Parser<char> = parseChar 'A'
let parseB: Parser<char> = parseChar 'B'

[<Test>]
let empty_string () =
    let expected: ParseResult<char * string> = Failure "No more input"
    "" |> run parseA |> should equal expected

[<Test>]
let when_char_is_found_return_message_and_rest_of_the_stream () =
    "Arest" |> run parseA |> should equal (Success(('A', "rest")))

[<Test>]
let when_char_is_not_found_return_false_and_unmodified_stream () =
    let expected: ParseResult<char * string> = Failure "Expecting 'A'. Got 'r'"
    "rest" |> run parseA |> should equal expected

[<Test>]
let only_char () =
    "A" |> run parseA |> should equal (Success('A', ""))

[<Test>]
let combine_parsers_with_andThen () =
    "ABC" |> run (andThen parseA parseB) |> should equal (Success(('A', 'B'), "C"))

[<Test>]
let andThen_when_parserA_fails () =
    let expected:ParseResult<(char * char) * string> = Failure "Expecting 'A'. Got 'Z'"
    "ZAB" |> run (andThen parseA parseB) |> should equal expected

[<Test>]
let andThen_when_parserB_fails () =
    let expected:ParseResult<(char * char) * string> = Failure "Expecting 'B'. Got 'Z'"
    "AZB" |> run (andThen parseA parseB) |> should equal expected

[<Test>]
let infix_operator () =
    "ABC" |> run (parseA .>>. parseB) |> should equal (Success(('A', 'B'), "C"))
