module ParserCombinators.Parser

type ParseResult<'a> =
    | Success of 'a
    | Failure of string

type Parser<'a> = Parser of (string -> ParseResult<'a * string>)

let run parser input =
    let (Parser inner) = parser
    inner input

let bind (f: 'a -> Parser<'b>) (p: Parser<'a>) : Parser<'b> =
    let inner input =
        let result = run p input

        match result with
        | Failure error -> Failure error
        | Success (value, rest) ->
            let parser = f value
            run parser rest

    Parser inner

let (>>=) p f = bind f p

let returnParser (value: 'a) : Parser<'a> =
    Parser(fun input -> Success(value, input))

let andThen (parserA: Parser<'a>) (parserB: Parser<'b>) : Parser<('a * 'b)> =
    parserA
    >>= (fun resultA -> parserB >>= (fun resultB -> (resultA, resultB) |> returnParser))

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

let rec map (func: 'a -> 'b) (parser: Parser<'a>) : Parser<'b> = parser >>= (func >> returnParser)

let (<!>) = map

let (|>>) value func = map func value

let apply (pFunc: Parser<('a -> 'b)>) (pValue: Parser<'a>) : Parser<'b> =
    pFunc >>= (fun f -> pValue >>= (fun value -> f value |> returnParser))

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
    parser
    >>= (fun head -> zeroOrMore parser >>= (fun rest -> head :: rest |> returnParser))

let optional (parser: Parser<'a>) : Parser<Option<'a>> =
    let some = parser |> map Some
    let none = returnParser None
    some <|> none

let leftOnly left right = left .>>. right |>> fst

let rightOnly left right = left .>>. right |>> snd

let (.>>) = leftOnly
let (>>.) = rightOnly

let between left middle right = left >>. middle .>> right

let separatedBy parser separator =
    parser .>>. zeroOrMore (separator >>. parser) |>> List.Cons
