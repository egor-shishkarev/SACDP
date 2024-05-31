module PhonebookInterface

open System
open PhonebookLogic

printfn("%s") "Добро пожаловать в программу - телефонный справочник. \nВыберите опцию из списка ниже"
let options = "
0 - выйти
1 - добавить запись
2 - найти запись по имени
3 - найти запись по телефону
4 - вывести все записи
5 - сохранить записи в файл
6 - считать данные из файла
"
printf("%s") <| options

let rec phonebookApp = 

    let phonebook: (string * string) list = []

    let rec cycle phonebook = 

        printf("%s") "\nВыберите опцию => "
        let option = Console.ReadLine()

        match option with
        | "0" -> 0
        | "1" ->
            printf("%s") "Введите имя => "
            let name = Console.ReadLine()
            printf("%s") "Введите номер => "
            let phone = Console.ReadLine()
            cycle (add phonebook name phone)
        | "2" ->
            printf("%s") "Введите имя => "
            let name = Console.ReadLine()
            printf("%s") (findByName phonebook name)
            cycle phonebook
        | "3" ->
            printf("%s") "Введите номер => "
            let phone = Console.ReadLine()
            printf("%s") (findByPhone phonebook phone)
            cycle phonebook
        | "4" -> 
            printf("%s") (toStringPhonebook phonebook)
            cycle phonebook
        | "5" ->
            save phonebook
            cycle phonebook
        | "6" ->
            cycle (getPhonebookFromFile phonebook)
        | _ -> 
            printf("%A") "Такой опции не существует!"
            cycle phonebook

    cycle phonebook

phonebookApp |> ignore
