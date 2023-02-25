module ParserCombinators.Test

open NUnit.Framework
open FsUnit

let parseChar char (stream: string) =
    if stream = "" then
        ("No more input", "")
    else
        let first = stream.[0]
        let rest = stream.[1..]

        if first = char then
            ( $"Found '{char}'", rest)
        else
            ($"Expecting '{char}'. Got '{first}'", stream)

[<Test>]
let empty_string () =
    "" |> parseChar 'A' |> should equal ("No more input", "")

[<Test>]
let when_char_is_found_return_message_and_rest_of_the_stream () =
    "Arest" |> parseChar 'A' |> should equal ("Found 'A'", "rest")

[<Test>]
let when_char_is_not_found_return_false_and_unmodified_stream () =
    "rest" |> parseChar 'A' |> should equal ("Expecting 'A'. Got 'r'", "rest")

[<Test>]
let only_char () =
    "A" |> parseChar 'A' |> should equal ("Found 'A'", "")
