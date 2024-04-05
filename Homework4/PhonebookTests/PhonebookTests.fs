module PhonebookTests

open NUnit.Framework
open FsUnit
open PhonebookLogic

[<Test>]
let ``Add should work as expected test`` () =
    let phonebook = [("Егор", "888888888")]
    add phonebook "Александр" "999999999" |> should equal [("Александр", "999999999"); ("Егор", "888888888")]

[<Test>]
let ``Find by name should work as expected test`` () =
    let phonebook = [("Егор", "888888888"); ("Александр", "999999999"); ("Антон", "777777777")]
    findByName phonebook "Антон" |> should equal "777777777"

[<Test>]
let ``Find by phone should work as expected test`` () =
    let phonebook = [("Егор", "888888888"); ("Александр", "999999999"); ("Антон", "777777777")]
    findByPhone phonebook "777777777" |> should equal "Антон"

[<Test>]
let ``To string phonebook should work as expected test`` () = 
    let phonebook = [("Егор", "888888888"); ("Александр", "999999999"); ("Антон", "777777777")]
    let expectedResult = "Name: Антон Phone: 777777777\nName: Александр Phone: 999999999\nName: Егор Phone: 888888888\n"
    toStringPhonebook phonebook |> should equal expectedResult