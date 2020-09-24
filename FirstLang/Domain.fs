module FirstLang.Domain

type Expression =
    | IntLiteral of int
    | FloatLiteral of float
    | StringLiteral of string
    | Identifier of string
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | And of Expression * Expression
    | Or of Expression * Expression
    | Equals of Expression * Expression
    | NotEquals of Expression * Expression
    | GreaterThan of Expression * Expression
    | GreaterThanOrEquals of Expression * Expression
    | LessThan of Expression * Expression
    | LessThanOrEquals of Expression * Expression

type OrderDirection =
    | Ascending
    | Descending

type Statement =
    | FilterBy of Expression
    | OrderBy of Expression * OrderDirection
    | Skip of int
    | Take of int
    | From of Expression

type Query = { Statements: Statement list }
