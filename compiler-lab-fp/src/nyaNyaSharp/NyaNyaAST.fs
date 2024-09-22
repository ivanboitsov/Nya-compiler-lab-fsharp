module NyaNyaAST

[<Literal>]
let LET = "meow"
[<Literal>]
let REC_LET = "_meow_"
[<Literal>]
let IF = "nya" 
[<Literal>]
let THEN = "nyanya"
[<Literal>]
let ELSE = "hss"
[<Literal>]
let FUN = "frr"
[<Literal>]
let ARROW = "UwU"
[<Literal>]
let R_APP = "muuur"

[<Literal>] 
let ASSIGN = "="

[<Literal>] 
let ADD = "+"
[<Literal>] 
let SUB = "-"
[<Literal>] 
let MUL = "*"
[<Literal>] 
let DIV = "/"
[<Literal>] 
let GT = ">" 
[<Literal>] 
let LT = "<" 
[<Literal>] 
let GE = ">=" 
[<Literal>] 
let LE = "<=" 
[<Literal>] 
let EQ = "=" 
[<Literal>]
let NEG = "!"

[<Literal>] 
let TRUE = "True"
[<Literal>] 
let FALSE = "False"
[<Literal>] 
let HEAD = "head"
[<Literal>] 
let TAIL = "tail"

type token = string
type expression = 
    | Var of token 
    | Int of int
    | Bool of bool
    | FLoat of float
    | String of token
    | Let of token * expression * expression
    | LetRec of token * expression * expression
    | Cond of expression * expression * expression
    | Lambda of token * expression
    | Application of expression * expression
    | Operaions of token * int * expression list
    | List of expression list
    | Closure of expression * env
    | RecClosure of expression * env * token
and env = Map<string, expression>