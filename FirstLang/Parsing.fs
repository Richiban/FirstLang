module FirstLang.Parsing

open FirstLang.Domain
open FParsec.CharParsers
open FParsec

module Keywords =
    let from = "from"
    let where = "where"
    let orderBy = "orderBy"
    let asc = "asc"
    let desc = "desc"
    let skip = "skip"
    let take = "take"

let ws = skipMany (skipChar ' ')
let atLeast1ws = skipMany1 (skipChar ' ')

let parseQuote = skipChar '\''

let parseStringLiteral =
    parseQuote
    >>. manyCharsTill anyChar parseQuote
    |>> StringLiteral
    .>> ws

let parseNumberLiteral =
    numberLiteral
        (NumberLiteralOptions.DefaultFloat
         ||| NumberLiteralOptions.DefaultInteger)
        "number"
    |>> fun n -> if n.IsInteger then IntLiteral(int n.String) else FloatLiteral(float n.String)
    .>> ws

let parseIdentifier =
    many1Chars (letter <|> digit)
    |>> Identifier
    .>> ws

let operators =
    OperatorPrecedenceParser<Expression, _, unit>()

operators.TermParser <-
    choice [ parseNumberLiteral
             parseStringLiteral
             parseIdentifier ]

[ InfixOperator("*", ws, 12, Associativity.Left, (fun x y -> Multiply(x, y)))
  InfixOperator("/", ws, 12, Associativity.Left, (fun x y -> Divide(x, y)))
  InfixOperator("+", ws, 10, Associativity.Left, (fun x y -> Add(x, y)))
  InfixOperator("-", ws, 10, Associativity.Left, (fun x y -> Subtract(x, y)))
  InfixOperator(">", ws, 8, Associativity.Left, (fun x y -> GreaterThan(x, y)))
  InfixOperator(">=", ws, 8, Associativity.Left, (fun x y -> GreaterThanOrEquals(x, y)))
  InfixOperator("<", ws, 8, Associativity.Left, (fun x y -> LessThan(x, y)))
  InfixOperator("<=", ws, 8, Associativity.Left, (fun x y -> LessThanOrEquals(x, y)))
  InfixOperator("==", ws, 4, Associativity.None, (fun x y -> Equals(x, y)))
  InfixOperator("/=", ws, 4, Associativity.None, (fun x y -> NotEquals(x, y)))
  InfixOperator("&", ws, 2, Associativity.Left, (fun x y -> And(x, y)))
  InfixOperator("|", ws, 2, Associativity.Left, (fun x y -> Or(x, y))) ]
|> Seq.iter operators.AddOperator

let parseExpression = operators.ExpressionParser

let parseWhere =
    skipString Keywords.where
    >>. atLeast1ws
    >>. parseExpression
    .>> ws
    |>> FilterBy

let parseOrderDirection =
    (skipString Keywords.asc >>% Ascending .>> ws)
    <|> (skipString Keywords.desc >>% Descending .>> ws)

let parseOrderBy =
    skipString Keywords.orderBy
    >>. atLeast1ws
    >>. parseExpression
    .>>. parseOrderDirection
    .>> ws
    |>> OrderBy

let parseSkip =
    skipString Keywords.skip
    >>. atLeast1ws
    >>. pint32
    .>> ws
    |>> Skip

let parseFrom =
    skipString Keywords.from
    >>. atLeast1ws
    >>. parseIdentifier
    .>> ws
    |>> From

let parseTake =
    skipString Keywords.take
    >>. atLeast1ws
    >>. pint32
    .>> ws
    |>> Take

let parseStatement =
    choice [ parseFrom
             parseWhere
             parseOrderBy
             parseSkip
             parseTake ]

let parseQuery =
    sepEndBy parseStatement skipNewline
    |>> fun s -> { Statements = s }

let parseQueryFull = spaces >>. parseQuery .>> spaces .>> eof

let parse input =
    match run parseQueryFull input with
    | Success (res, _, _) -> Result.Ok res
    | Failure (err, _, _) -> Result.Error err
