module ParserCombinators.Test.ZeroOrMoreTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let returns_success_for_empty_list () =
    let empty: ParseResult<char list * string> = Success([], "")
    run (zeroOrMore parseA) "" |> should equal empty

[<Test>]
let returns_success_for_single_result () =
    "A" |> run (zeroOrMore parseA) |> should equal <| Success([ 'A' ], "")

[<Test>]
let multiple_results () =
    let expected: ParseResult<char list * string> = Success([ 'A'; 'A' ], "")
    "AA" |> run (zeroOrMore parseA) |> should equal expected

[<Test>]
let returns_success_and_recursively_parse_until_failure () =
    "AABA" |> run (zeroOrMore parseA) |> should equal <| Success([ 'A'; 'A' ], "BA")

[<Test>]
let match_multiple_strings () =
    let parser =
        parseString "AB"
        |> zeroOrMore
    
    "ABABCC" |> run parser  |> should equal <| Success(["AB"; "AB"], "CC")
