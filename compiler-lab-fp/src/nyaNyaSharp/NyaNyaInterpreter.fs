module NyaNyaInterpreter

open NyaNyaAST

// Минимальное количество аргументов для операций
let argsNum = function
    | "&" | "|" | "+" | "-" | "*" | "/" | "==" | "!=" | ">" | "<" | ">=" | "<=" | "[]" | "append" -> 2
    | "!" -> 1
    | _ -> failwith "Неизвестная операция"

// Выполнение операций
let performOperation op args =
    match op, args with
    | "[]", [List(list); Int(n)] -> list.[n]
    | "append", [List(list); x] -> List(list @ [x])
    | "&", [Bool(a); Bool(b)] -> Bool(a && b)
    | "|", [Bool(a); Bool(b)] -> Bool(a || b)
    | "not", [Bool(a)] -> Bool(not a)
    | "+", [Float(a); Float(b)] -> Float(a + b)
    | "-", [Float(a); Float(b)] -> Float(a - b)
    | "*", [Float(a); Float(b)] -> Float(a * b)
    | "/", [Float(a); Float(b)] -> Float(a / b)
    | ">", [Float(a); Float(b)] -> Bool(a > b)
    | "<", [Float(a); Float(b)] -> Bool(a < b)
    | ">=", [Float(a); Float(b)] -> Bool(a >= b)
    | "<=", [Float(a); Float(b)] -> Bool(a <= b)
    | "==", [Float(a); Float(b)] -> Bool(a = b)
    | "!=", [Float(a); Float(b)] -> Bool(a <> b)
    | _ -> failwith "Неверная операция или аргументы"

let rec eval exp env =
    match exp with
    | Application(e1, e2) -> apply (eval e1 env) (eval e2 env)
    | Bool(n) -> Bool(n)
    | Int(n) -> Int(n)
    | Float(n) -> Float(n)
    | List(n) -> List(List.map (fun x -> eval x env) n)
    | Var(x) -> Map.find x env
    | PrimOp(f) -> Operations(f, argsNum f, [])
    | Cond(cond, yes, no) ->
        if eval cond env = Bool(true) then eval yes env else eval no env
    | Operations(id, n, el) -> Operations(id, n, el)
    | Let(id, e1, e2) ->
        let r = eval e1 env
        eval e2 (Map.add id r env)
    | LetRec(id, e1, e2) ->
        eval e2 (Map.add id (RecClosure(e1, env, id)) env)
    | Lambda(param, body) -> Closure(Lambda(param, body), env)
    | Closure(Lambda(param, body), env) -> exp
    | Prog(exp_list) ->
        if List.isEmpty exp_list then None
        else exp_list |> List.map (fun x -> eval x env) |> List.last
    | Print(x) ->
        let res = eval x env
        match res with
        | Float(n) -> printfn "%A" n
        | Int(n) -> printfn "%A" n
        | Bool(n) -> printfn "%A" n
        | List(n) -> printfn "%A" n
        | String(s) -> printfn "%s" s  // Поддержка вывода строк
        | _ -> printfn "the type does not support printing"
        None
    | _ -> exp

and apply e1 e2 = 
        match e1 with
        | Closure(Lambda(param,body),env) -> 
            match e2 with
                |List(value_list) -> 
                    value_list
                    let env_add = List.zip param value_list |> Map.ofList
                    let new_env = Map.fold(fun acc key value -> Map.add key value acc) env env_add
                    eval body new_env
        | RecClosure(Lambda(param,body),env, id) -> 
            match e2 with
                |List(value_list) -> 
                    value_list
                    let env_add = List.zip param value_list |> Map.ofList
                    let new_env = Map.fold(fun acc key value -> Map.add key value acc) env env_add
                    eval body (Map.add id e1 new_env)
        | Operations(id,n,args) ->
            if n=1 then (performOperation id)(args@[e2])
            else Operations(id, n-1, args@[e2])

let exec exp = eval exp Map.empty
