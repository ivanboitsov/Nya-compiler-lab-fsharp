module NyaNyaParser

open FParsec.Primitives
open FParsec.CharParsers
open NyaNyaAST

// Парсер для чисел (Int)
let pInt: Parser<Expr, unit> = 
    pint32 |>> Int

// Парсер для булевых значений (Bool)
let pBool: Parser<Expr, unit> = 
    (pstring "true" >>% Bool true) <|> (pstring "false" >>% Bool false)

// Парсер для строк (String)
let pString: Parser<Expr, unit> =
    between (pchar '"') (pchar '"') (manyChars (noneOf "\"")) |>> String

// Парсер для переменных (Var)
let pVar: Parser<Expr, unit> = 
    many1SatisfyL isLetter "variable" |>> Var

// Парсер для ключевого слова 'Meow'
let pMeowKeyword = pstring "Meow" .>> spaces

// Парсер для конструкции 'Meow x = 42 in <expr>'
let pMeow: Parser<Expr, unit> = 
    pMeowKeyword >>. spaces1 >>. pVar .>> spaces .>> pstring "=" .>> spaces .>>. pExpr .>> spaces .>> pstring "in" .>> spaces .>>. pExpr
    |>> fun ((Var name, value), body) -> Meow(name, value, body)

// Парсер для ключевого слова 'Woem'
let pWoemKeyword = pstring "Woem" .>> spaces

// Парсер для конструкции 'Woem x = 42 in <expr>'
let pWoem: Parser<Expr, unit> = 
    pWoemKeyword >>. spaces1 >>. pVar .>> spaces .>> pstring "=" .>> spaces .>>. pExpr .>> spaces .>> pstring "in" .>> spaces .>>. pExpr
    |>> fun ((Var name, value), body) -> Woem(name, value, body)

// Парсер для лямбда-выражения 'UwU x => <expr>'
let pUwU: Parser<Expr, unit> = 
    pstring "UwU" >>. spaces1 >>. pVar .>> spaces .>> pstring "=>" .>> spaces .>>. pExpr
    |>> fun (Var name, body) -> UwU(name, body)

// Парсер для условного выражения 'Nya <cond> nyaNya <thenExpr> hss <elseExpr>'
let pNyaKeyword = pstring "Nya" .>> spaces
let pNyaNyaKeyword = pstring "nyaNya" .>> spaces
let pHssKeyword = pstring "hss" .>> spaces

let pNya: Parser<Expr, unit> =
    pNyaKeyword >>. pExpr .>> pNyaNyaKeyword .>>. pExpr .>> pHssKeyword .>>. pExpr
    |>> fun ((cond, thenExpr), elseExpr) -> Nya(cond, thenExpr, elseExpr)

// Парсер для списка выражений (Cats)
let pCats: Parser<Expr, unit> =
    between (pchar '[') (pchar ']') (sepBy pExpr (pchar ',' >> spaces)) |>> Cats

// Парсер для бинарных операций (Meowing)
let pMeowingOp = pstring "+" <|> pstring "-" <|> pstring "*" <|> pstring "/"

let pMeowing: Parser<Expr, unit> = 
    pExpr .>> spaces .>>. pMeowingOp .>> spaces .>>. pExpr
    |>> fun ((left, op), right) -> Meowing(op, left, right)

// Парсер для вызова функции (Frr)
let pFrr: Parser<Expr, unit> = 
    pExpr .>> spaces1 .>>. pExpr
    |>> fun (func, arg) -> Frr(func, arg)

// Главный парсер выражений с рекурсивными привязками
let rec pExpr, pExprImpl = createParserForwardedToRef<Expr, unit>()

do pExprImpl := choice [
    attempt pMeow
    attempt pWoem
    attempt pNya
    attempt pUwU
    attempt pMeowing
    attempt pFrr
    pInt
    pBool
    pString
    pVar
    pCats
]

// Функция для запуска парсера
let parseInput (input: string) =
    match run pExpr input with
    | Success(result, _, _) -> Result.Ok result
    | Failure(errorMsg, _, _) -> Result.Error errorMsg
