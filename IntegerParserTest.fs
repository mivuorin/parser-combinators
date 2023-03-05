module ParserCombinators.Test.IntegerParserTest

open System
open Microsoft.FSharp.Core
open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

let parseInt: Parser<int> =
    let digit = anyOf ['0'..'9']
    let oneOrMoreDigits = oneOrMore digit

    oneOrMoreDigits
    |> map (fun chars -> chars |> List.toArray |> String |> int)

[<Test>]
let single_digit () =
    let expected: ParseResult<int * string> = Success(1, "ABC")
    "1ABC" |> run parseInt |> should equal expected

[<Test>]
let multiple_digits () =
    let expected: ParseResult<int * string> = Success(12, "BC")
    "12BC" |> run parseInt |>  should equal expected

[<Test>]
let failure_message_is_not_best_one () =
    let expected: ParseResult<int * string> = failureNotExpected '9' 'A'
    "A" |> run parseInt |>  should equal expected
    
    