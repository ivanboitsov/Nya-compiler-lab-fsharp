module NyaNyaInterpreter

open NyaNyaAST

// The value that the interpreter will return (representing the evaluation result)
type value =
    | IntVal of int
    | FloatVal of float
    | BoolVal of bool
    | StringVal of string
    | ListVal of value list
    | ClosureVal of token * expression * env
    | RecClosureVal of token * expression * env * token
and env = Map<string, value>

// Helper function to look up variables in the environment
let lookup (env: env) (var: string) =
    match Map.tryFind var env with
    | Some value -> value
    | None -> failwithf "Variable '%s' not found" var

// Core evaluation function
let rec eval (env: env) (expr: expression) : value =
    match expr with
    | Var v -> lookup env v

    | Int n -> IntVal n
    | FLoat f -> FloatVal f
    | Bool b -> BoolVal b
    | String s -> StringVal s

    | Let (name, valueExpr, body) ->
        let value = eval env valueExpr
        let newEnv = Map.add name value env
        eval newEnv body

    | LetRec (name, valueExpr, body) ->
        // Handle recursive let by introducing the variable to the environment
        let recEnv = ref env
        let value = eval !recEnv valueExpr
        let newEnv = Map.add name value env
        recEnv := newEnv
        eval newEnv body

    | Cond (condExpr, thenExpr, elseExpr) ->
        let condValue = eval env condExpr
        match condValue with
        | BoolVal true -> eval env thenExpr
        | BoolVal false -> eval env elseExpr
        | _ -> failwith "Condition must evaluate to a boolean"

    | Lambda (param, body) -> ClosureVal (param, body, env)

    | Application (fnExpr, argExpr) ->
        let fnValue = eval env fnExpr
        let argValue = eval env argExpr
        match fnValue with
        | ClosureVal (param, body, closureEnv) ->
            let newEnv = Map.add param argValue closureEnv
            eval newEnv body
        | RecClosureVal (param, body, closureEnv, fnName) ->
            let newEnv = Map.add param argValue (Map.add fnName fnValue closureEnv)
            eval newEnv body
        | _ -> failwith "Expected a function in application"

    | Operaions (op, arity, args) ->
        let evaluatedArgs = List.map (eval env) args
        evalOp op arity evaluatedArgs

    | List exprs ->
        let values = List.map (eval env) exprs
        ListVal values

and evalOp op arity args =
    // Handle various operations (+, -, *, /, etc.)
    match (op, args) with
    | (ADD, [IntVal l; IntVal r]) -> IntVal (l + r)
    | (SUB, [IntVal l; IntVal r]) -> IntVal (l - r)
    | (MUL, [IntVal l; IntVal r]) -> IntVal (l * r)
    | (DIV, [IntVal l; IntVal r]) -> IntVal (l / r)
    | (ADD, [FloatVal l; FloatVal r]) -> FloatVal (l + r)
    | (SUB, [FloatVal l; FloatVal r]) -> FloatVal (l - r)
    | (MUL, [FloatVal l; FloatVal r]) -> FloatVal (l * r)
    | (DIV, [FloatVal l; FloatVal r]) -> FloatVal (l / r)
    | (GT, [IntVal l; IntVal r]) -> BoolVal (l > r)
    | (LT, [IntVal l; IntVal r]) -> BoolVal (l < r)
    | (GE, [IntVal l; IntVal r]) -> BoolVal (l >= r)
    | (LE, [IntVal l; IntVal r]) -> BoolVal (l <= r)
    | (EQ, [IntVal l; IntVal r]) -> BoolVal (l = r)
    | _ -> failwithf "Unsupported operation or incorrect arity: %s" op

// Helper function to run the entire program
let runProgram (input: string) =
    match NyaNyaParser.parseInput input with
    | Result.Ok expr ->
        let initialEnv = Map.empty
        let result = eval initialEnv expr
        printfn "Result: %A" result
    | Result.Error msg -> printfn "Parse error: %s" msg
