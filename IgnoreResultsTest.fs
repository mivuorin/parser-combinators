module ParserCombinators.Test.IgnoreResultsTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

let parseQuote = parseChar '"'

[<Test>]
let leftOnly_sequences_both_parsers_but_keeps_only_result_from_left_parser () =
    let parser = leftOnly parseA parseB
    let expected = Success('A', "")
    "AB" |> run parser |> should equal expected

[<Test>]
let leftOnly_failure () =
    let parser = leftOnly parseA parseB
    let expected: ParseResult<char * string> = failureNotExpected 'B' 'C'
    "AC" |> run parser |> should equal expected

[<Test>]
let rightOnly_sequences_both_parsers_but_keeps_only_result_from_right_parser () =
    let parser = rightOnly parseA parseB
    let expected = Success('B', "")
    "AB" |> run parser |> should equal expected

[<Test>]
let rightOnly_failure () =
    let parser = rightOnly parseA parseB
    let expected: ParseResult<char * string> = failureNotExpected 'B' 'C'
    "AC" |> run parser |> should equal expected

[<Test>]
let infix_operators () =
    let parser = parseQuote >>. parseA .>>. parseB .>> parseQuote
    "\"AB\"" |> run parser |> should equal <| Success(('A', 'B'), "")

[<Test>]
let between_should_ignore_left_and_right_sides_and_return_middle () =
    let parser = between parseQuote parseA parseQuote
    "\"A\"" |> run parser |> should equal <| Success('A', "")

[<Test>]
let whitespace_example () =
    let whitespace = anyOf [ ' '; '\t'; '\n' ]
    let parser = parseA .>> zeroOrMore whitespace
    "A  \t\n" |> run parser |> should equal <| Success('A', "")
