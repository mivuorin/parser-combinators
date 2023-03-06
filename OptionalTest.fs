module ParserCombinators.Test.OptionalTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let return_some_when_parser_succeeds () =
    let parser = optional parseA
    let expected: ParseResult<char option * string> = Success(((Some 'A'), ""))
    "A" |> run parser |> should equal expected

[<Test>]
let return_success_with_none_when_parser_fails () =
    let parser = optional parseA
    let expected: ParseResult<char option * string> = Success(None, "B")
    "B" |> run parser |> should equal expected
