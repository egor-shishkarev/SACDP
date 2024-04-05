module PhonebookTests

open NUnit.Framework
open FsUnit
open PhonebookLogic

[<Test>]
let ``Add should work as expected test`` () =
    let phonebook = [("����", "888888888")]
    add phonebook "���������" "999999999" |> should equal [("���������", "999999999"); ("����", "888888888")]

[<Test>]
let ``Find by name should work as expected test`` () =
    let phonebook = [("����", "888888888"); ("���������", "999999999"); ("�����", "777777777")]
    findByName phonebook "�����" |> should equal "777777777"

[<Test>]
let ``Find by phone should work as expected test`` () =
    let phonebook = [("����", "888888888"); ("���������", "999999999"); ("�����", "777777777")]
    findByPhone phonebook "777777777" |> should equal "�����"

[<Test>]
let ``To string phonebook should work as expected test`` () = 
    let phonebook = [("����", "888888888"); ("���������", "999999999"); ("�����", "777777777")]
    let expectedResult = "Name: ����� Phone: 777777777\nName: ��������� Phone: 999999999\nName: ���� Phone: 888888888\n"
    toStringPhonebook phonebook |> should equal expectedResult