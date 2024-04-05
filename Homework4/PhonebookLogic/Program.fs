module PhonebookLogic

open System.IO

    // 1 - add
    // 2 - findByName
    // 3 - findByPhone
    // 4 - printPhonebook
    // 5 - save
    // 6 - getPhonebookFromFile

let filePath = "../../../phonebook.txt"

let add phonebook name phone = [name, phone] :: phonebook

let findByName (phonebook: (string * string) list) name = 
    let person = List.tryFind (fun item -> fst item = name) phonebook
    if person.IsSome then
        snd person.Value
    else
        ""
    
let findByPhone (phonebook: (string * string) list) phone = 
    let person = List.tryFind (fun item -> snd item = phone) phonebook
    if person.IsSome then
        fst person.Value
    else
        ""

let toStringPhonebook (phonebook: (string * string) list) = 
    let rec innerFunction phonebook result =
        match phonebook with
        | [] -> result + "\n"
        | h :: t -> innerFunction t result + "\n" + "Name: " + fst h + " Phone: " + snd h 
    innerFunction phonebook ""

let save (phonebook: (string * string) list) =
    let rec innerFunction phonebook result = 
        match phonebook with
        | [] -> result
        | h :: t -> innerFunction t (String.concat "\n" [result; fst h + "," + snd h])
    let data = innerFunction phonebook ""
    File.WriteAllLines(filePath, [data])

let getPhonebookFromFile = 
    let newData = Array.toList (File.ReadAllLines filePath)
    let newPhonebook = List.map (fun (item: string) -> [item.Split(",")[0], item.Split(",")[1]]) newData
    newPhonebook


