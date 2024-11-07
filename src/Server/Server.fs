module Server

open SAFE
open Saturn
open Shared
open System
open Npgsql.FSharp

// let connectionString : string =
//     Sql.host "localhost"
//     |> Sql.database (Environment.GetEnvironmentVariable"DATABASE")
//     |> Sql.username (Environment.GetEnvironmentVariable "USERNAME")
//     |> Sql.password (Environment.GetEnvironmentVariable "PASS")
//     |> Sql.port (Environment.GetEnvironmentVariable "PORT" |> int)
//     |> Sql.formatConnectionString

let connectionString : string =
    Sql.host "localhost"
    |> Sql.database "tarotDB"
    |> Sql.username "postgres"
    |> Sql.password "postgres"
    |> Sql.port 5432
    |> Sql.formatConnectionString

let getZodiacByName (connectionString: string) (zodiacName: Result<string, ZodiacValidateError>) : Result<Fortune list,string> =
    match zodiacName with 
    | Ok zodiacName -> 
        try 
            let result =
                connectionString
                |> Sql.connect
                |> Sql.query "SELECT * FROM zodiac_table WHERE zodiac = @zodiacName"
                |> Sql.parameters [ "zodiacName", Sql.string zodiacName ]
                |> Sql.execute (fun read ->
                    {
                        Zodiac = read.text "zodiac"
                        Elemental = read.text "elemental"
                        Description = read.text "description"
                    })
            Ok result
        with
        | msg -> Error $"เกิดข้อผิดพลาดในการดึงข้อมูล : {msg.Message}"
    | Error _ -> Error "ชื่อราศีไม่ถูกต้อง"

let getZodiacByBirthdate (birthdate:int)(birthmonth:string) : Result<string, ZodiacValidateError> =
    if birthmonth = "มกราคม" && birthdate >= 1 && birthdate <= 13 then Ok "ธนู"
    elif birthmonth = "มกราคม" && birthdate >= 14 && birthdate <= 31 then Ok "มังกร"
    elif birthmonth = "กุมภาพันธ์" && birthdate >= 1 && birthdate <= 12 then Ok "มังกร"
    elif birthmonth = "กุมภาพันธ์" && birthdate >= 13 && birthdate <= 29 then Ok "กุมภ์"
    elif birthmonth = "มีนาคม" && birthdate >= 1 && birthdate <= 13 then Ok "กุมภ์"
    elif birthmonth = "มีนาคม" && birthdate >= 14 && birthdate <= 31 then Ok "มีน"
    elif birthmonth = "เมษายน" && birthdate >= 1 && birthdate <= 14 then Ok "มีน"
    elif birthmonth = "เมษายน" && birthdate >= 15 && birthdate <= 30 then Ok "เมษ"
    elif birthmonth = "พฤษภา" && birthdate >= 1 && birthdate <= 14 then Ok "เมษ"
    elif birthmonth = "พฤษภา" && birthdate >= 15 && birthdate <= 30 then Ok "พฤษภ"
    elif birthmonth = "มิถุนายน" && birthdate >= 1 && birthdate <= 14 then Ok "พฤษภ"
    elif birthmonth = "มิถุนายน" && birthdate >= 15 && birthdate <= 30 then Ok "มิถุน"
    elif birthmonth = "กรกฎาคม" && birthdate >= 1 && birthdate <= 15 then Ok "มิถุน"
    elif birthmonth = "กรกฎาคม" && birthdate >= 16 && birthdate <= 31 then Ok "กรกฎ"
    elif birthmonth = "สิงหาคม" && birthdate >= 1 && birthdate <= 16 then Ok "กรกฎ"
    elif birthmonth = "สิงหาคม" && birthdate >= 17 && birthdate <= 31 then Ok "สิงห์"
    elif birthmonth = "กันยายน" && birthdate >= 1 && birthdate <= 15 then Ok "สิงห์"
    elif birthmonth = "กันยายน" && birthdate >= 16 && birthdate <= 30 then Ok "กันย์"
    elif birthmonth = "ตุลาคม" && birthdate >= 1 && birthdate <= 16 then Ok "กันย์"
    elif birthmonth = "ตุลาคม" && birthdate >= 17 && birthdate <= 31 then Ok "ตุลย์"
    elif birthmonth = "พฤศจิกายน" && birthdate >= 1 && birthdate <= 15 then Ok "ตุลย์"
    elif birthmonth = "พฤศจิกายน" && birthdate >= 16 && birthdate <= 30 then Ok "พิจิก"
    elif birthmonth = "ธันวาคม" && birthdate >= 1 && birthdate <= 15 then Ok "พิจิก"
    elif birthmonth = "ธันวาคม" && birthdate >= 16 && birthdate <= 31 then Ok "ธนู"
    else Error (ZodiacError NoReason)

let getNumberByRandom =
    let card = [1 .. 22]
    let random = Random()
    let index = random.Next(0, Seq.length card)
    // printfn "Number By Random is %d" index
    Seq.item index card 

// printfn "Number By Random is %d" getNumberByRandom

let getCardByPick (connectionString: string) (card_id:int) : Result<Card List,string> =
    try
        let result =
            connectionString
            |> Sql.connect
            |> Sql.query "SELECT * FROM card_table WHERE card_id = @card_id" 
            |> Sql.parameters ["card_id", Sql.int card_id]
            |> Sql.execute (fun read ->
                {
                    CardId = read.int "card_id"
                    CardName = read.text "card_name"
                    CardDescription = read.text "card_description"
                    CardLove = read.text "card_love"
                    CardWork = read.text "card_work"
                    CardMoney = read.text "card_money"
                    CardTotal = read.text "card_total"
                })
        printfn $"No problem is here {result} and card name is {card_id}"
        Ok result
    with
    | msg -> Error $"เกิดข้อผิดพลาดในการดึงข้อมูล : {msg.Message}"

let getCardByView (connectionString: string) (card_id:int) : Result<Card List,string> =
    // let card_name =
    //     if card_name.Length <> 32 then card_name + String.replicate (32 - card_name.Length) " "
    //     else card_name
    try
        let result =
            connectionString
            |> Sql.connect
            |> Sql.query "SELECT * FROM card_table WHERE card_id = @card_id" 
            |> Sql.parameters ["card_id", Sql.int card_id]
            |> Sql.execute (fun read ->
                {
                    CardId = read.int "card_id"
                    CardName = read.text "card_name"
                    CardDescription = read.text "card_description"
                    CardLove = read.text "card_love"
                    CardWork = read.text "card_work"
                    CardMoney = read.text "card_money"
                    CardTotal = read.text "card_total"
                })
        if result = [] then printfn $"No Data is here {result} and card name is {card_id}."
        else printfn $"No problem is here {result} and card name is {card_id}."
        Ok result
    with
    | msg -> Error $"เกิดข้อผิดพลาดในการดึงข้อมูล : {msg.Message}"

let getCardByFavorite (connectionString: string) : Result<Room List,string> =
    try
        let result =
            connectionString
            |> Sql.connect
            |> Sql.query "SELECT * FROM favorite_table" 
            |> Sql.execute (fun read ->
                {
                    Id = read.int "id"
                    Date = read.text "day"
                    CardName = read.text "card_name"
                })
        if  result = [] then Error "ไม่มีข้อมูล" 
        else Ok result
    with
    | msg -> Error $"เกิดข้อผิดพลาดในการดึงข้อมูล : {msg.Message}"

// let getCardByFavorite (connectionString: string)  =
//     connectionString
//     |> Sql.connect
//     |> Sql.query "SELECT * FROM favorite_table" 
//     |> Sql.execute (fun read ->
//         {
//             Id = read.int "id"
//             Date = read.text "day"
//             CardName = read.text "card_name"
//         })



let addCardtoFav (connectionString: string)(id: int,date:string,card:string) : Result<string,string>=
    try
        connectionString
        |> Sql.connect
        |> Sql.query "INSERT INTO favorite_table (id,day,card_name) VALUES (@id,@date, @card)"
        |> Sql.parameters ["@id", Sql.int id;"@date", Sql.string date ; "@card", Sql.string card]
        |> Sql.execute (fun read ->
            {

                Id = read.int "id"
                Date = read.text "day"
                CardName = read.text "card_name"
            })
        |> ignore 
        Ok "บันทึกไพ่นี้ลงในคลังที่ชื่นชอบเรียบร้อย"
    with
    | msg -> Error $"เกิดข้อผิดพลาดในการบันทึกข้อมูล: {msg.Message}"

let updateCardtoFav (connectionString: string)(id:int,date:string,card:string) : Result<string,string>=
    try
        connectionString
        |> Sql.connect
        |> Sql.query "UPDATE favorite_table SET id = @id,card_name = @card, day = @date WHERE id = 1"
        |> Sql.parameters ["@id", Sql.int id;"@date", Sql.string date ; "@card", Sql.string card]
        |> Sql.execute (fun read ->
            {
                Id = read.int "id"
                Date = read.text "day"
                CardName = read.text "card_name"
            })
        |> ignore 
        Ok "บันทึกไพ่นี้ลงในคลังที่ชื่นชอบเรียบร้อย"
    with
    | msg -> Error $"เกิดข้อผิดพลาดในการบันทึกข้อมูล: {msg.Message}"

let checkCardExists (connectionString: string) (day: string) : Result<bool, string> =
    try
        let result =
            connectionString
            |> Sql.connect
            |> Sql.query "SELECT COUNT(*) AS count FROM favorite_table WHERE day = @day"
            |> Sql.parameters ["@day", Sql.string day]
            |> Sql.execute (fun read -> read.int "count")
        
        match result with
        | [count] when count > 0 ->  printf "Count = %d" count;Ok true
        | _ -> Ok false
    with
    | ex -> Error $"เกิดข้อผิดพลาดในการตรวจสอบข้อมูล: {ex.Message}"

let addOrUpdateCard (connectionString: string) (id: int,date: string, card: string) : Result<string, string> =
    match checkCardExists connectionString date with
    | Ok true ->
        updateCardtoFav connectionString (id,date, card)
    | Ok false ->
        addCardtoFav connectionString (id,date, card)
    | Error err ->
        Error err

let deleteCardDavorite (connectionString: string) =
    connectionString
    |> Sql.connect
    |> Sql.query "DELETE FROM favorite_table WHERE true"
    |> Sql.executeNonQuery
    |> ignore



let tarotApi ctx = {
    getZodiac = fun (birthdate: int,birthmonth: string) -> async {
        let zodiacResult = getZodiacByBirthdate birthdate birthmonth
        match getZodiacByName connectionString zodiacResult with
        | Ok zodiacList -> return zodiacList
        | Error _ -> return []
    }

    getCard = fun () -> async {
        match getCardByPick connectionString getNumberByRandom with
        | Ok card -> return card
        | Error _ -> return []
        
    }

    // getFavoriteCard = fun () -> async {
    //     let card = getCardByFavorite connectionString
    //     return card
    // }

    getFavoriteCard = fun () -> async {
        match getCardByFavorite connectionString with
        | Ok card -> return card
        | Error _ -> return []
    }

    getViewCard = fun (card_id:int) -> async {
        match getCardByPick connectionString card_id with
        | Ok card -> return card
        | Error _ -> return []
    }

    addOrupdateCard = fun (id:int,date: string, card: string) -> async {
        match addOrUpdateCard connectionString (id,date, card) with
        | Ok msg -> return msg
        | Error msg -> return msg
    }

    deleteCard = fun () -> async {
        deleteCardDavorite connectionString
        return "Success"
    }
                
}


let webApp = Api.make tarotApi

let app = application {
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
}

[<EntryPoint>]
let main _ =
    run app
    0