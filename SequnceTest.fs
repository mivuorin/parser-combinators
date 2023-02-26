module ParserCombinators.Test.SequenceTest

open System
open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser


let parseString (string: string) : Parser<string> =
    string
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |> map (fun chars -> chars |> List.toArray |> String)

[<Test>]
let sequence_combines_multiple_parsers_into_single_one () =
    let parsers = [ parseA; parseB; parseC ]
    let combined = sequence parsers
    "ABCD" |> run combined |> should equal (Success([ 'A'; 'B'; 'C' ], "D"))


[<Test>]
let parseString_test () =
    let parser: Parser<string> = parseString "ABC"
    "ABCDE" |> run parser |> should equal (Success("ABC", "DE"))
