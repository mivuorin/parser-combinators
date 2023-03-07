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
            | Success (valueB, rest) -> Success(((valueA, valueB), rest))

    Parser inner

let (.>>.) = andThen

let orElse (parserA: Parser<'a>) (parserB: Parser<'a>) : Parser<'a> =
    let inner input =
        let result = run parserA input

        match result with
        | Failure _ -> run parserB input
        | error -> error

    Parser inner

let (<|>) = orElse

let choice parsers = List.reduce orElse parsers

let rec map (func: 'a -> 'b) (parser: Parser<'a>) : Parser<'b> =
    let inner input =
        let result = run parser input

        match result with
        | Success (value, rest) ->
            let mapped = func value
            Success(mapped, rest)
        | Failure error -> Failure error

    Parser inner

let (<!>) = map

let (|>>) value func = map func value

let returnParser (value: 'a) : Parser<'a> =
    Parser(fun input -> Success(value, input))

let apply (func: Parser<('a -> 'b)>) (value: Parser<'a>) : Parser<'b> =
    let paired = (func .>>. value)
    paired |> map (fun (func, value) -> func value)

let (<*>) = apply

let lif2 f a b = returnParser f <*> a <*> b

let rec sequence (parsers: Parser<'a> list) : Parser<'a list> =
    let cons = lif2 (fun head rest -> head :: rest)

    match parsers with
    | [] -> returnParser []
    | head :: rest -> cons head (sequence rest)

let rec private recurZeroOrMore parser input : 'a list * string =
    let result = run parser input

    match result with
    | Failure _ -> ([], input)
    | Success (value, rest) ->
        let values, rest = recurZeroOrMore parser rest
        (value :: values, rest)

let zeroOrMore parser : Parser<'a list> =
    let inner input = Success(recurZeroOrMore parser input)
    Parser inner

let oneOrMore (parser: Parser<'a>) : Parser<'a list> =
    let inner (input: string) =
        let first = run parser input

        match first with
        | Failure error -> Failure error
        | Success (value, rest) ->
            let values, rest = recurZeroOrMore parser rest
            Success(value :: values, rest)

    Parser inner

let optional (parser: Parser<'a>) : Parser<Option<'a>> =
    let some = parser |> map Some
    let none = returnParser None
    some <|> none

let leftOnly left right =
    left .>>. right |>> fst

let rightOnly left right =
    left .>>. right |>> snd

let (.>>) = leftOnly
let (>>.) = rightOnly

let between left middle right =
    left >>. middle .>> right
