module PhonebookLogic

open System.IO

let filePath = "../../../phonebook.txt"

let add phonebook name phone = (name, phone) :: phonebook

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
        | [] -> result
        | h :: t -> innerFunction t "Name: " + fst h + " Phone: " + snd h + "\n" + result
    innerFunction phonebook ""

let save (phonebook: (string * string) list) =
    let rec innerFunction phonebook result = 
        match phonebook with
        | [] -> result
        | h :: t -> innerFunction t (fst h + "," + snd h + "\n" + result)
    let data = innerFunction phonebook ""
    File.WriteAllLines(filePath, [data])

let getPhonebookFromFile (phonebook: (string * string) list) = 
    let newData = List.filter(fun item -> item <> "") (Array.toList (File.ReadAllLines filePath))
    
    let newPhonebook = List.map (fun (item: string) -> (item.Split(",")[0], item.Split(",")[1])) newData
    phonebook @ newPhonebook

