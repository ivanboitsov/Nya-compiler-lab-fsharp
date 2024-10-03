module NyaNyaAST

type token = string
type expression = 
    | Var of token 
    | Int of int
    | Bool of bool
    | Float of float
    | String of token
    | PrimOp of token
    | List of expression list
    | Let of token * expression * expression
    | LetRec of token * expression * expression
    | Cond of expression * expression * expression
    | Operations of token * int * expression list
    | Application of expression * expression
    | Lambda of token list * expression
    | Closure of expression * env
    | RecClosure of expression * env * token
    | Print of expression
    | Prog of expression list
    | None
and env = Map<string, expression>