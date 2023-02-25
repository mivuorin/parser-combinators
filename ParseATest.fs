module ParserCombinators.Test

open NUnit.Framework
open FsUnit

let parseChar char (stream: string) =
    if stream = "" then
        (false, "")
    else
        let first = stream.[0]
        let rest = stream.[1..]
        if first = char then
            (true, rest)
        else
            (false, stream)

[<Test>]
let empty_string () =
    "" |> parseChar 'A' |> should equal (false, "")

[<Test>]
let when_char_is_found_return_true_and_rest_of_the_stream () =
    "Arest" |> parseChar 'A' |> should equal (true, "rest")

[<Test>]
let when_char_is_not_found_return_false_and_unmodified_stream () =
    "rest" |> parseChar 'A' |> should equal (false, "rest")

[<Test>]
let only_char () =
    "A" |> parseChar 'A' |> should equal (true, "")
