module ParserCombinators.Test.MapTest

open System
open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

let parseThreeDigits =
    let parser = parseDigit .>>. parseDigit .>>. parseDigit
    let toString ((a, b), c) = String [| a; b; c |]
    map toString parser

[<Test>]
let map_when_parser_succeeds () =
    let parser = map Char.ToLower parseA
    "A" |> run parser |> should equal (Success('a', ""))

[<Test>]
let map_when_parser_fails () =
    let parser = map Char.ToLower parseA
    let expected: ParseResult<char * string> = error 'A' 'B'
    "B" |> run parser |> should equal expected

[<Test>]
let map_infix_operator () =
    let parser = Char.ToLower <!> parseA
    "A" |> run parser |> should equal (Success('a', ""))

[<Test>]
let map_convenience_infix_operator_with_reversed_parameters () =
    let parser = parseA |>> Char.ToLower
    "A" |> run parser |> should equal (Success('a', ""))

[<Test>]
let parseThreeDigits_should_map_result_to_string () =
    "123" |> run parseThreeDigits |> should equal (Success("123", ""))

[<Test>]
let parseThreeDigits_mapped_to_int () =
    "123" |> run (parseThreeDigits |>> int) |> should equal (Success(123, ""))
