module ParserCombinators.CharacterParser

open System
open ParserCombinators.Parser

let failureNotExpected expected actual =
    Failure $"Expecting '{expected}'. Got '{actual}'"

let failureNoMoreInput  =
    Failure "No more input"

let parseChar char =
    let parser stream =
        if stream = "" then
            Failure "No more input"
        else
            let first = stream.[0]
            let rest = stream.[1..]

            if first = char then
                Success(char, rest)
            else
                failureNotExpected char first

    Parser parser

let parseA: Parser<char> = parseChar 'A'
let parseB: Parser<char> = parseChar 'B'
let parseC: Parser<char> = parseChar 'C'

let anyOf chars = chars |> List.map parseChar |> choice

let parseLowercase = anyOf [ 'a' .. 'z' ]

let parseDigit = anyOf [ '1' .. '9' ]

let parseString (string: string) : Parser<string> =
    string
    |> List.ofSeq
    |> List.map parseChar
    |> sequence
    |> map (fun chars -> chars |> List.toArray |> String)