module ParserCombinators.Test.ApplyTest

open System
open NUnit.Framework
open FsUnit
open ParserCombinators.Parser

let startsWith (input:string) (prefix:string) =
    input.StartsWith(prefix)

[<Test>]
let returnParser_creates_parser_from_input () =
    let parser = returnParser 'A'
    "foo" |> run parser |> should equal (Success('A' , "foo"))

[<Test>]
let apply_maps_function_inside_parser_to_value_in_other_parser () =
    let func:Parser<char -> char> = returnParser Char.ToUpper
    let value:Parser<char> = returnParser 'a'
    let actual = apply func value
    "" |> run actual |> should equal (Success('A', ""))