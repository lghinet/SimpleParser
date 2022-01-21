namespace Evaluation.ExpressionsParser

open FParsec

type BinaryExpressionKind = 
    | Add
    | Subtract
    | Divide
    | Multiply
    | And
    | Or
    | Equals
    | NotEquals
    | GreaterThan
    | GreaterThanOrEquals
    | LesserThan
    | LesserThanOrEquals


type Expr = 
    | IntLiteral of int
    | FloatLiteral of float
    | BoolLiteral of bool
    | StringLiteral of string
    | Identifier of string
    | Variable of string
    | Binary of (Expr * Expr * BinaryExpressionKind)

module StringExpressionsParser =
    let ws = spaces
    let str_ws s = pstring s >>. ws
    let quote = skipChar '"'
    let stringLiteral = quote >>. manyCharsTill anyChar quote |>> Expr.StringLiteral .>> ws
    let boolLiteral = (pstring "true" <|> pstring "false") |>> System.Boolean.Parse |>> Expr.BoolLiteral .>> ws
    
    let identifier = many1Chars (letter <|> digit) |>> Expr.Identifier .>> ws
    let variable = many1Chars2 (pchar '@') (letter <|> digit) |>> Expr.Variable .>> ws
    
    let numberLiteral = 
        numberLiteral (NumberLiteralOptions.DefaultFloat ||| NumberLiteralOptions.DefaultInteger) "number"  
        |>> fun n ->
                if n.IsInteger then int n.String |> Expr.IntLiteral
                else float n.String |> Expr.FloatLiteral
        .>> ws
    
    let opp = OperatorPrecedenceParser<Expr, unit, unit>()
    
    opp.AddOperator(InfixOperator("*", ws, 12, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Multiply)))
    opp.AddOperator(InfixOperator("/", ws, 11, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Divide)))
    opp.AddOperator(InfixOperator("-", ws, 10, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Subtract)))
    opp.AddOperator(InfixOperator("+", ws, 9, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Add)))
    opp.AddOperator(InfixOperator("==", ws, 8, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Equals)))
    opp.AddOperator(InfixOperator("!=", ws, 7, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.NotEquals)))
    opp.AddOperator(InfixOperator(">=", ws, 6, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.GreaterThanOrEquals)))
    opp.AddOperator(InfixOperator(">", ws, 5, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.GreaterThan)))
    opp.AddOperator(InfixOperator("<", ws, 4, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.LesserThan)))
    opp.AddOperator(InfixOperator("<=", ws, 3, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.LesserThanOrEquals)))
    opp.AddOperator(InfixOperator("&", ws, 2, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.And)))
    opp.AddOperator(InfixOperator("|", ws, 1, Associativity.Left, fun x y -> Expr.Binary (x,y,BinaryExpressionKind.Or)))
    
    opp.TermParser <- choice [
            boolLiteral
            stringLiteral  
            numberLiteral
            identifier 
            variable
            between (str_ws "(") (str_ws ")") opp.ExpressionParser
        ]
    
    let RunParser = run opp.ExpressionParser