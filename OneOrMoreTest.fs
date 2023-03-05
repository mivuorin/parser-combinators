module ParserCombinators.Test.OneOrMoreTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let return_failure_for_empty_input () =
    let expected: ParseResult<char list * string> = failureNoMoreInput
    let parser: Parser<char list> = parseA |> oneOrMore
    run parser "" |> should equal expected

[<Test>]
let return_failure_on_first_input () =
    let expected: ParseResult<char list * string> = failureNotExpected 'A' 'B'
    let parser: Parser<char list> = parseA |> oneOrMore
    run parser "B" |> should equal expected

[<Test>]
let return_success_for_single_result () =
    let parser = parseA |> oneOrMore
    "A" |> run parser |> should equal <| Success([ 'A' ], "")

[<Test>]
let multiple_results () =
    let expected: ParseResult<char list * string> = Success([ 'A'; 'A' ], "")
    let parser = parseA |> oneOrMore
    "AA" |> run parser |> should equal expected

[<Test>]
let return_success_and_recursively_parse_until_failure () =
    let parser = parseA |> oneOrMore
    "AABA" |> run parser |> should equal <| Success([ 'A'; 'A' ], "BA")
