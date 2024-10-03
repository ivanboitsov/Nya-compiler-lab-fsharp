open System
open NyaNyaParser
open NyaNyaInterpreter

let printCat () =
    printfn @"       \`-._           __"
    printfn @"        \\  `-..____,.'  `."
    printfn @"         :`.         /    \`."
    printfn @"         :  )       :      : \"
    printfn @"          ;'        '   ;  |  :"
    printfn @"          )..      .. .:.`.;  :"
    printfn @"         /::...  .:::...   ` ;"
    printfn @"         ; _ '    __        /:\"
    printfn @"         `:o>   /\o_>      ;:. `."
    printfn @"        `-`.__ ;   __..--- /:.   \"
    printfn @"        === \_/   ;=====_.':.     ;"
    printfn @"         ,/'`--'...`--....        ;"
    printfn @"              ;                    ;"
    printfn @"            .'                      ;"
    printfn @"          .'                        ;"
    printfn @"        .'     ..     ,      .       ;"
    printfn @"       :       ::..  /      ;::.     |"
    printfn @"      /      `.;::.  |       ;:..    ;"
    printfn @"     :         |:.   :       ;:.    ;"
    printfn @"     :         ::     ;:..   |.    ;"
    printfn @"      :       :;      :::....|     |"
    printfn @"      /\     ,/ \      ;:::::;     ;"
    printfn @"    .:. \:..|    :     ; '.--|     ;"
    printfn @"   ::.  :''  `-.,,;     ;'   ;     ;"
    printfn @" .-'. _.'\      / `;      \,__:      \"
    printfn @" `---'    `----'   ;      /    \,.,,,/"
    printfn @"                   `----`"

let printSuccessCat () =     
    printfn @"   /\_/\  ("
    printfn @"  ( ^.^ ) _)"
    printfn @"    \'/  ("
    printfn @"  ( | | )"
    printfn @" (__d b__)"

let printErrorCat () =
    printfn @"    /\_____/\ "
    printfn @"   /  o   o  \"
    printfn @"  ( ==  ^  == )"
    printfn @"   )         ("
    printfn @"  (           )"
    printfn @" ( (  )   (  ) )"
    printfn @"(__(__)___(__)__)"


// Функция для работы с консолью: получение строк и завершение по Ctrl+D
let rec readInput (input: string list) =
    printf "> "
    match Console.ReadLine() with
    | null -> input // Завершение по Ctrl+D (Linux/Unix) или Ctrl+Z (Windows)
    | line -> readInput (input @ [line])

// Основной цикл программы
let rec mainLoop () =
    printCat()
    printfn "Введите код (завершите ввод с помощью Ctrl+Z или введите 'exit' для выхода):"
    let input = readInput [] |> String.concat "\n"
    
    if input = "exit" then
        printfn "Программа завершена."
    else
        // Интерпретация введенного кода
        match NyaNyaParser.parseInput input with
        | Result.Ok expr ->
            let initialEnv = Map.empty
            let result = eval expr initialEnv
            printfn "Программа скомпилированна успешно"
            printSuccessCat()
        | Result.Error msg -> 
            printfn "Ошибка парсинга: %s" msg
            printErrorCat()


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