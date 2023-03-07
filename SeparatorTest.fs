module ParserCombinators.Test.SeparatorTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let separatedBy_parses_list_with_one_or_more_items_separated_by_delimiter () =
    let parser = parseChar ',' |> separatedBy parseInt
    "1,2,3" |> run parser |> should equal <| Success([ 1; 2; 3 ], "")

[<Test>]
let separatedBy_empty_list () =
    let parser = parseChar ',' |> separatedBy parseInt
    let parseResult: ParseResult<int list * string> = failureNoMoreInput
    "" |> run parser |> should equal <| parseResult
