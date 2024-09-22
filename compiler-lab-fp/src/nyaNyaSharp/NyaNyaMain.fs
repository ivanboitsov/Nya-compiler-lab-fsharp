open System
open NyaNyaParser
open NyaNyaInterpreter

// Функция для работы с консолью: получение строк и завершение по Ctrl+D
let rec readInput (input: string list) =
    printf "> "
    match Console.ReadLine() with
    | null -> input // Завершение по Ctrl+D (Linux/Unix) или Ctrl+Z (Windows)
    | line -> readInput (input @ [line])

// Основной цикл программы
let rec mainLoop () =
    printfn "Введите код (завершите ввод с помощью Ctrl+Z или введите 'exit' для выхода):"
    let input = readInput [] |> String.concat "\n"
    
    if input = "exit" then
        printfn "Программа завершена."
    else
        // Интерпретация введенного кода
        match NyaNyaParser.parseInput input with
        | Result.Ok expr ->
            let initialEnv = Map.empty
            let result = eval initialEnv expr
            printfn "Результат: %A" result
        | Result.Error msg -> printfn "Ошибка парсинга: %s" msg

        // Ожидание нажатия пробела для продолжения
        printfn "Нажмите пробел для продолжения или 'exit' для завершения."
        let key = Console.ReadKey()
        if key.Key = ConsoleKey.Spacebar then
            mainLoop () // Повтор цикла
        else
            printfn "\nПрограмма завершена."

// Запуск основного цикла
[<EntryPoint>]
let main _ =
    mainLoop ()
    0