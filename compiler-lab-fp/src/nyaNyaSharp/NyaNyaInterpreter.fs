module NyaNyaInterpreter

open NyaNyaAST

// Окружение для хранения значений переменных
type Env = Map<string, Expr>

// Функция для интерпретации выражений
let rec eval (env: Env) (expr: Expr) : Expr =
    match expr with
    | Int _ | Bool _ | Float _ | String _ -> expr // Литералы остаются без изменений

    | Var x ->
        match Map.tryFind x env with
        | Some value -> value
        | None -> failwithf "Неизвестная переменная: %s" x

    | Meow (name, valueExpr, bodyExpr) ->
        let value = eval env valueExpr
        let newEnv = Map.add name value env
        eval newEnv bodyExpr

    | Woem (name, valueExpr, bodyExpr) ->
        let intermediateEnv = ref env
        let value = eval !intermediateEnv valueExpr
        let newEnv = Map.add name value env
        eval newEnv bodyExpr

    | UwU (param, body) ->
        UwU(param, body) // Вернем замыкание

    | Nya (condExpr, thenExpr, elseExpr) ->
        let condValue = eval env condExpr
        match condValue with
        | Bool true -> eval env thenExpr
        | Bool false -> eval env elseExpr
        | _ -> failwith "Условие должно быть булевым выражением"

    | Frr (funcExpr, argExpr) ->
        let func = eval env funcExpr
        let arg = eval env argExpr
        match func with
        | UwU (param, body) ->
            let newEnv = Map.add param arg env
            eval newEnv body
        | _ -> failwith "Выражение не является функцией"

    | Meowing (op, leftExpr, rightExpr) ->
        let left = eval env leftExpr
        let right = eval env rightExpr
        match left, right with
        | Int l, Int r ->
            match op with
            | "+" -> Int (l + r)
            | "-" -> Int (l - r)
            | "*" -> Int (l * r)
            | "/" -> Int (l / r)
            | _ -> failwithf "Неизвестная операция: %s" op
        | _ -> failwith "Операция поддерживается только для целых чисел"

    | Cats exprList ->
        Cats (List.map (eval env) exprList)
