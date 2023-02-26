module ParserCombinators.Test.ParserTest

open NUnit.Framework
open FsUnit
open ParserCombinators.Parser
open ParserCombinators.CharacterParser

[<Test>]
let empty_string () =
    let expected: ParseResult<char * string> = Failure "No more input"
    "" |> run parseA |> should equal expected

[<Test>]
let when_char_is_found_return_message_and_rest_of_the_stream () =
    "Arest" |> run parseA |> should equal (Success(('A', "rest")))

[<Test>]
let when_char_is_not_found_return_false_and_unmodified_stream () =
    let expected: ParseResult<char * string> = Failure "Expecting 'A'. Got 'r'"
    "rest" |> run parseA |> should equal expected

[<Test>]
let only_char () =
    "A" |> run parseA |> should equal (Success('A', ""))

[<Test>]
let combine_parsers_with_andThen () =
    "ABC" |> run (andThen parseA parseB) |> should equal (Success(('A', 'B'), "C"))

[<Test>]
let andThen_when_parserA_fails () =
    let expected: ParseResult<(char * char) * string> = Failure "Expecting 'A'. Got 'Z'"
    "ZAB" |> run (andThen parseA parseB) |> should equal expected

[<Test>]
let andThen_when_parserB_fails () =
    let expected: ParseResult<(char * char) * string> = Failure "Expecting 'B'. Got 'Z'"
    "AZB" |> run (andThen parseA parseB) |> should equal expected

[<Test>]
let andThen_infix_operator () =
    "ABC" |> run (parseA .>>. parseB) |> should equal (Success(('A', 'B'), "C"))

[<Test>]
let orElse_when_parserA_succeeds () =
    "AZZ" |> run (orElse parseA parseB) |> should equal (Success('A', "ZZ"))

[<Test>]
let orElse_when_parserA_fails_and_parserB_succeeds () =
    let expected = Success('B', "ZZ")
    "BZZ" |> run (orElse parseA parseB) |> should equal expected

[<Test>]
let orElse_when_parserA_fails_and_parserB_fails () =
    let expected: ParseResult<char * string> = Failure("Expecting 'B'. Got 'C'")
    "CZZ" |> run (orElse parseA parseB) |> should equal expected

[<Test>]
let orElse_infix_operator () =
    "AZZ" |> run (parseA <|> parseB) |> should equal (Success('A', "ZZ"))

[<Test>]
let combine_andThen_and_orElse () =
    let bOrC = parseB <|> parseC
    let aAndThenBorC = parseA .>>. bOrC
    "AB" |> run aAndThenBorC |> should equal (Success(('A', 'B'), ""))
    "AC" |> run aAndThenBorC |> should equal (Success(('A', 'C'), ""))

[<Test>]
let choice_executes_parsers_until_one_fails_or_passes () =
    let parseAorBorC = choice [ parseA; parseB; parseC ]
    "A" |> run parseAorBorC |> should equal (Success('A', ""))
    "B" |> run parseAorBorC |> should equal (Success('B', ""))
    "C" |> run parseAorBorC |> should equal (Success('C', ""))

[<Test>]
let parseLowercase_implemented_with_anyOf_and_choice () =
    "aBC" |> run parseLowercase |> should equal (Success('a', "BC"))

[<Test>]
let parseDigit_implemented_with_anyOf_and_choice () =
    "1BC" |> run parseDigit |> should equal (Success('1', "BC"))