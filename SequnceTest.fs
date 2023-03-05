module ParserCombinators.Test.SequenceTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let sequence_combines_multiple_parsers_into_single_one () =
    let parsers = [ parseA; parseB; parseC ]
    let combined = sequence parsers
    "ABCD" |> run combined |> should equal (Success([ 'A'; 'B'; 'C' ], "D"))


[<Test>]
let parseString_test () =
    let parser: Parser<string> = parseString "ABC"
    "ABCDE" |> run parser |> should equal (Success("ABC", "DE"))
