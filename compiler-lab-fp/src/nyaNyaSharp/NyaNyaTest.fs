module NyaNyaTest

open NyaNyaAST
open NyaNyaInterpreter

// Примеры выражений на языке Nya
let testExpressions () =
    let env = Map.empty

    let expressions = [
        "Meow x = 42 in x", Meow("x", Int 42, Var "x")
        "Meow y = 10 in Meowing(+, y, Int 2)", Meow("y", Int 10, Meowing("+", Var "y", Int 2))
        "Woem fact = UwU n => Nya(Bool false, Int 1, Meowing(*, Var n, Frr(Var fact, Meowing(-, Var n, Int 1)))) in Frr(Var fact, Int 5)",
            Woem("fact", UwU("n", Nya(Bool false, Int 1, Meowing("*", Var "n", Frr(Var "fact", Meowing("-", Var "n", Int 1))))), Frr(Var "fact", Int 5))
        "Nya(Bool true, Int 1, Int 0)", Nya(Bool true, Int 1, Int 0)
    ]

    for (input, expr) in expressions do
        printfn "Testing: %s" input
        let result = eval env expr
        printfn "Result: %A" result
        printfn "-----------------------"

testExpressions()