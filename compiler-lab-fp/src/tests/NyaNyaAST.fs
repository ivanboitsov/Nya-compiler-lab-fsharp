module NyaNyaAST

type Expr = 
    | Var of string 
    | Int of int
    | Bool of bool
    | Float of float
    | String of string
    | Meow of string * Expr * Expr // Определение (имя + значение + аргумент)
    | Woem of string * Expr * Expr // Рекурсивное определение
    | UwU of string * Expr // Лямбда выражение
    | Nya of Expr * Expr * Expr // Условное выражение
    | Frr of Expr * Expr // Функция
    | Cats of Expr list 
    | Meowing of string * Expr * Expr // Бинарные операции