module ParserCombinators.Test.IntegerParserTest

open Microsoft.FSharp.Core
open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

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

[<Test>]
let optional_minus_sign () =
    let expected: ParseResult<int * string> = Success(-23, "A")
    "-23A" |> run parseInt |>  should equal expected