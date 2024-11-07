module Testfunction

open SAFE
open Expecto
open System

let connectionString : string =
    Sql.host "localhost"
    |> Sql.database (Environment.GetEnvironmentVariable"DATABASE")
    |> Sql.username (Environment.GetEnvironmentVariable "USERNAME")
    |> Sql.password (Environment.GetEnvironmentVariable "PASS")
    |> Sql.port (Environment.GetEnvironmentVariable "PORT" |> int)
    |> Sql.formatConnectionString

[<Tests>]
let testAddCardtoFav =
    testCase "Test adding card to favorite_card table" <| fun _ ->
        // กำหนดข้อมูลที่จะทดสอบการเพิ่ม
        let testDate = DateTime.Now.ToString("yyyy-MM-dd")
        let testCard = "Test Card Name"

        // เรียกใช้ฟังก์ชัน addCardtoFav
        addCardtoFav connectionString (testDate, testCard)

        // ตรวจสอบว่าข้อมูลถูกเพิ่มลงฐานข้อมูลสำเร็จ
        let checkQuery =
            connectionString
            |> Sql.connect
            |> Sql.query "SELECT COUNT(*) FROM favorite_card WHERE day = @date AND card = @card"
            |> Sql.parameters [ "@date", Sql.string testDate; "@card", Sql.string testCard ]
            |> Sql.executeScalar
            |> unbox<int>

        Expect.equal checkQuery 1 "Record should be inserted into favorite_card table"

[<EntryPoint>]
let main argv =
    runTestsInAssembly defaultConfig argv