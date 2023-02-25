module ParserCombinators.Parser

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>) 

let run parser input =
    let (Parser inner) = parser
    inner input

let andThen (parserA: Parser<'a>) (parserB: Parser<'b>) : Parser<('a * 'b)> =
    let inner input =
        let result = run parserA input
        match result with
        | Failure error -> Failure error
        | Success (valueA, rest) ->
            let resultB = run parserB rest
            match resultB with
            | Failure error -> Failure error
            | Success (valueB, rest) ->
                Success (((valueA, valueB), rest))
    Parser inner

let ( .>>. ) = andThen

let orElse (parserA: Parser<'a>) (parserB: Parser<'a>) : Parser<'a> =
    let inner input =
        let result = run parserA input
        match result with
        | Failure _ ->
            run parserB input
        | error -> error
        
    Parser inner

let ( <|> ) = orElse
