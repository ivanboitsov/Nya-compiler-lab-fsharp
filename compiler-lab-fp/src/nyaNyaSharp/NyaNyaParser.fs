module NyaNyaParser

open FParsec
open NyaNyaAST

// Перенаправляем вызовы к парсеру через ссылку
let pExpr, pExprRef = createParserForwardedToRef<expression, unit> ()

// Парсер для строковых идентификаторов
let pIdentifier: Parser<string, unit> =
    many1Satisfy2L isLetter (fun c -> isLetter c || isDigit c) "identifier"

let pToken : Parser<string, unit> =
    let isVarFirstChar c = isLetter c || c = '_'
    let isVarChar c = isLetter c || isDigit c || c = '_'
    many1Satisfy2 isVarFirstChar isVarChar |>> token

let pVar : Parser<expression, unit> =
    pToken |>> Var

let pInt : Parser<expression, unit> =
    pint32 .>> notFollowedBy(satisfy (fun c -> c = '.')) |>> Int

let pFloat : Parser<expression, unit> = 
    pfloat |>> FLoat

let pBool : Parser<expression, unit> =
    (stringReturn TRUE true) <|> (stringReturn FALSE false) |>> Bool

let stringLiteral =
    let escape =  anyOf "\"\\/bfnrt"
                  |>> function
                      | 'b' -> "\b"
                      | 'f' -> "\u000C"
                      | 'n' -> "\n"
                      | 'r' -> "\r"
                      | 't' -> "\t"
                      | c   -> string c

    let unicodeEscape =
        
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
            (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
            |> char |> string
        )

    let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet  = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let pString : Parser<expression, unit> =
    stringLiteral |>> String 

let pLet : Parser<expression, unit> = 
    pipe3 (pstring LET >>. spaces >>. pToken .>> spaces) 
          (pstring ASSIGN >>. spaces >>. pExpr .>> spaces)
          (pExpr) 
          (fun a b c -> Let(a, b, c))

let pLetrec : Parser<expression, unit> = 
    pipe3 (pstring REC_LET >>. spaces >>. pToken .>> spaces) 
          (pstring ASSIGN >>. spaces >>. pExpr .>> spaces)
          (pExpr) 
          (fun a b c -> LetRec(a, b, c))

let ops = [ADD; SUB ; MUL; DIV; GE; LE; GT; LT; EQ; HEAD; TAIL; NEG]

let pCond : Parser<expression, unit> = 
    pipe3 (pstring IF >>. spaces >>. pExpr .>> spaces) (pstring THEN >>. spaces >>. pExpr .>> spaces)
            (pstring ELSE >>. spaces >>. pExpr) (fun a b c -> Cond(a, b ,c))

let pLambda : Parser<expression, unit> = 
    pipe2 (pstring FUN >>. spaces >>. pToken .>> spaces) (pstring ARROW >>. spaces >>. pExpr) 
        (fun a b -> Lambda(a, b))

let pApp : Parser<expression, unit> = 
    pipe2 (pstring R_APP >>. spaces >>. pExpr .>> spaces) (pExpr) (fun a b -> Application(a, b))

let listBetweenStrings sOpen sClose pElement f =
    between (pstring sOpen) (pstring sClose)
            (spaces >>. sepBy (pElement .>> spaces) (pstring "," >>. spaces) |>> f)

let pList = listBetweenStrings "[" "]" pExpr List

do pExprRef.Value <- choice [
    pApp
    pLambda
    pCond
    pLet
    pLetrec
    pList
    pString
    pVar
    attempt pInt
    pFloat
]


let fprogram : Parser<expression, unit> = spaces >>. pExpr .>> spaces .>> eof

let testParser p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

// Функция для запуска парсера
let parseInput (input: string) = 
    match run fprogram input with
    | Success(result, _, _) -> Result.Ok result
    | Failure(errorMsg, errorState, _) ->
        let errorPosition = errorState.Position
        Result.Error (sprintf "Error at line %d, column %d: %s" errorPosition.Line errorPosition.Column errorMsg)
