module NyaNyaParser

open FParsec
open NyaNyaAST

let pExpr, pExprRef = createParserForwardedToRef()

let pExprFloat = pfloat .>> spaces |>> Float

let pBool =
    pstring "true" <|> pstring "false"  .>> spaces |>> 
        function
            | "true" -> Bool(true)
            | "false" -> Bool(false)

let pToken =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "invalid variable or function pToken" .>> spaces |>> Var

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

let pString = stringLiteral |>> String

let Operator = choice[
                    pstring "&" |>> fun _ -> "&"
                    pstring "|" |>> fun _ -> "|"
                    pstring "not" |>> fun _ -> "not"
                    pstring "+" |>> fun _ -> "+"
                    pstring "-" |>> fun _ -> "-"
                    pstring "*" |>> fun _ -> "*"
                    pstring "/" |>> fun _ -> "/"
                    pstring "==" |>> fun _ -> "=="
                    pstring "!=" |>> fun _ -> "!="
                    pstring ">" |>> fun _ -> ">"
                    pstring "<" |>> fun _ -> "<"
                    pstring ">=" |>> fun _ -> ">="
                    pstring "<=" |>> fun _ -> "<="
                    ]

let pVar = 
    pstring "meow" >>. spaces >>. pToken .>>. 
    (spaces >>. skipChar '=' >>. spaces >>. pExpr) .>> spaces .>>. 
    many pExpr .>> spaces|>> 
        fun ((Var(pToken), value), expr_list) -> Let(pToken, value, Prog(expr_list)) 

let pMeth = 
    pToken .>> spaces .>>.
    (skipChar '.' >>. pToken .>> spaces) .>>.
    (skipChar '(' >>. many pExpr .>> spaces .>> skipChar ')' .>> spaces) |>>
        fun((list, Var(method)), args) -> 
            let acc = Application(PrimOp(method), list)
            List.fold (fun acc x -> Application(acc, x)) acc args 
            

let pCond =
    (pstring "nya" >>. spaces >>. pExpr .>> spaces) .>>.
    (pstring "uw" >>. spaces >>. skipChar '{' >>. spaces >>. many pExpr .>> spaces .>> skipChar '}' .>> spaces) .>>.
    (pstring "hss" >>. spaces >>. skipChar '{' >>. spaces >>. many pExpr .>> spaces .>> skipChar '}' .>> spaces) |>>
    fun ((cond, res), alt) -> Cond(cond, Prog(res), Prog(alt))

let pList = 
    skipChar '[' >>. spaces >>. many pExpr .>> spaces .>> skipChar ']' .>> spaces |>> List

let pIndexList = 
    pToken .>>.
    (spaces >>. skipChar '[' >>. spaces >>. pint32 .>> spaces .>> skipChar ']' .>> spaces) |>>
        fun(list, num) -> 
            Application(Application(PrimOp("[]"), list), Int(num))

let pFunction =
    (pstring "frr" >>. spaces >>. pToken) .>>. 
    (skipChar '(' >>. spaces  >>. many pToken .>> skipChar ')') .>>. 
    (spaces >>. skipChar '{' >>. spaces >>. many pExpr .>> spaces .>> skipChar '}' .>> spaces) .>>. 
    (many pExpr .>> spaces) |>> 
        fun(((Var(func), params), body), expr_list) -> 
            
            let ids_extracted = 
                params |> List.map(
                    function
                        |Var(x) -> x 
            )

            Let(func, Lambda(ids_extracted, Prog(body)), Prog(expr_list))

let pFunctionRec =
    (pstring "fur" >>. spaces >>. pToken) .>>. 
    (skipChar '(' >>. spaces  >>. many pToken .>> skipChar ')') .>>. 
    (spaces >>. skipChar '{' >>. spaces >>. many pExpr .>> spaces .>> skipChar '}' .>> spaces) .>>. 
    (many pExpr .>> spaces) |>> 
        fun(((Var(func), params), body), expr_list) -> 
            
            let ids_extracted = 
                params |> List.map(
                    function
                        |Var(x) -> x 
            )

            LetRec(func, Lambda(ids_extracted, Prog(body)), Prog(expr_list))

let pFunCall =
    pToken .>>. 
    (spaces >>. skipChar '(' >>. spaces >>. many pExpr .>> spaces .>> skipChar ')' .>> spaces) |>>
        fun(func, arg) -> Application(func, List(arg))

let pOperation = 
    (pExprFloat <|> attempt pFunCall <|> pToken <|> pBool) .>> spaces .>>. 
    Operator .>> spaces .>>. 
    (pExprFloat <|> attempt pFunCall <|> pToken <|> pBool) .>> spaces |>> 
        fun ((e1, op), e2) -> 
            Application(Application(PrimOp(op),  e1),  e2)

let pExprPrint =
    pstring "sayNya" >>. spaces >>. skipChar '(' >>. spaces >>. many pExpr .>> spaces .>> skipChar ')' .>> spaces |>> fun(arg) -> Print(Prog(arg))

pExprRef := choice[
    pString;
    pFunctionRec; 
    pFunction; 
    pExprPrint; 
    pCond; 
    attempt pOperation;  
    attempt pFunCall; 
    attempt pIndexList; 
    attempt pMeth; 
    pVar; pList; 
    pExprFloat; 
    pBool; 
    pToken;
]

let final = spaces >>. many pExpr .>> eof |>> Prog

// Функция для запуска парсера
let parseInput (input: string) = 
    match run pExpr input with
    | Success(result, _, _) -> Result.Ok result
    | Failure(errorMsg, errorState, _) ->
        let errorPosition = errorState.Position
        Result.Error (sprintf "Error at line %d, column %d: %s" errorPosition.Line errorPosition.Column errorMsg)