module Index

open Elmish
open SAFE
open Shared
open Browser.Types

type Model = {
        Zodiac: RemoteData<Fortune List>
        Card: RemoteData<Card list>
        FavCard: RemoteData<Room list>
        DelCard: string
        SelectedDay: int
        SelectedMonth: string
        SelectedCardName: string
        showZodiac: bool
        showModal: bool
        SelectedId: int
        SelectDate: string
        SelectCard: string
        AddAlready: string
        IsFavCardLoaded: bool
        validationMessage: string option

}

type Msg = 
    | LoadDestiny of ApiCall<int*string, Fortune List>
    | LoadCard of ApiCall<unit,Card List>
    | LoadFavCard of ApiCall<unit,Room List>
    | LoadViewCard of ApiCall<int,Card List>
    | AddCard of ApiCall<int*string*string,string>
    | DeleteCard of ApiCall<unit,string>
    | SetDate of int
    | SetMonth of string
    | SetCardId of int
    | ShowModal 
    | CloseModal
    | LoadCardAndShowModal
    | LoadViewCardAndShowModal


let tarotApi = Api.makeProxy<ICardApi> ()
let init () =
    let initialModel = {
        Zodiac = NotStarted;
        FavCard = NotStarted;
        DelCard = "";
        SelectedDay = 1;
        SelectedMonth = "มกราคม";
        SelectedCardName = "";
        showZodiac = false;
        showModal = false;
        Card = NotStarted;
        SelectedId = 0
        SelectDate = "";
        SelectCard = "";
        AddAlready = "";
        IsFavCardLoaded = false;
        validationMessage = None;
    }

    let initialCmd = LoadFavCard(Start()) |> Cmd.ofMsg
    initialModel, initialCmd



let update msg model =
    match msg with
    | SetDate day -> { model with SelectedDay = day},Cmd.none

    | SetMonth month -> { model with SelectedMonth = month},Cmd.none

    | SetCardId card_id -> {model with SelectedId= card_id}, Cmd.none

    | LoadDestiny msg ->
        match msg with
        | Start(selectedDay,selectedMonth) ->
            let loadDestinyCmd = Cmd.OfAsync.perform tarotApi.getZodiac (selectedDay,selectedMonth) (Finished >> LoadDestiny)
            { model with Zodiac = Loading; showZodiac = false }, loadDestinyCmd
        | Finished zodiac -> { model with Zodiac = Loaded zodiac; showZodiac = true }, Cmd.none

    | LoadCard msg ->
        match msg with
        | Start() ->
            let loadCardCmd = Cmd.OfAsync.perform tarotApi.getCard () (Finished >> LoadCard)
            { model with Card = Loading}, loadCardCmd
        | Finished card -> { model with Card = Loaded card;},Cmd.none

    | LoadFavCard msg ->
        match msg with
        | Start() ->
            let loadFavCardCmd = Cmd.OfAsync.perform tarotApi.getFavoriteCard () (Finished >> LoadFavCard)
            { model with FavCard = Loading; }, loadFavCardCmd
        | Finished favcard -> { model with FavCard = Loaded favcard},Cmd.none

    |LoadViewCard msg ->
        match msg with
        | Start(card_id) ->
            let loadViewCardCmd = Cmd.OfAsync.perform tarotApi.getViewCard (card_id) (Finished >> LoadCard)
            { model with Card = Loading}, loadViewCardCmd
        | Finished card -> { model with Card = Loaded card;},Cmd.none
    
    | DeleteCard msg ->
        match msg with 
        | Start() ->
            let deleteCardCmd = Cmd.OfAsync.perform tarotApi.deleteCard () (Finished >> DeleteCard)
            { model with FavCard = Loading}, deleteCardCmd
        | Finished favcard -> { model with DelCard = favcard},Cmd.none



    | ShowModal -> { model with showModal = true}, Cmd.none

    | CloseModal -> { model with showModal = false}, Cmd.none

    | LoadCardAndShowModal ->
        let loadCardCmd = Cmd.ofMsg (LoadCard (Start ()))
        let showModalCmd = Cmd.ofMsg ShowModal
        model, Cmd.batch [ loadCardCmd; showModalCmd]

    | LoadViewCardAndShowModal ->
        if model.SelectedId <> 0 then 
            let loadViewCardCmd = Cmd.ofMsg (LoadViewCard(Start(model.SelectedId)))
            let showModalCmd = Cmd.ofMsg ShowModal
            model, Cmd.batch [ loadViewCardCmd; showModalCmd]
        else {model with showModal = true}, Cmd.none

    | AddCard msg ->
        match msg with
        | Start(selectId,selectDate,selectCard) ->
            let addcardCmd = Cmd.OfAsync.perform tarotApi.addOrupdateCard (selectId,selectDate,selectCard) (Finished >> AddCard)
            { model with FavCard = Loading}, addcardCmd
        | Finished fav -> { model with SelectCard = fav},Cmd.none


open Feliz

module ViewComponents = 
    let welCome  = 
        Html.div [
            prop.className "flex flex-col m-5"
            prop.children [
                Html.h1 [
                    prop.className "text-center text-5xl font-bold text-purple-800 mb-3 rounded-md p-4"
                    prop.text "°*:ยินดีต้อนรับสู่โลกของทาโรต์ ✧*"
                        
                    ]
                Html.h3 [
                    prop.className "text-center text-4xl font-bold text-purple-800 mb-3 rounded-md p-4"
                    prop.text "°*Kunmom Tarot ✧*"
                ]
            ]
        ]
    let endFooter  = 
        Html.div [
            prop.className "flex flex-col m-5"
            prop.children [
                Html.h1 [
                    prop.className "text-center text-2xl font-bold "
                    prop.text "°*:รับดูดวงเพื่อไขข้อกังวล🔮🪄 ✧*"
                        
                    ]
                Html.h3 [
                    prop.className "text-center text-xl font-bold  p-4"
                    prop.text "°* << Kunmom Tarot ✧*"
                ]
            ]
        ]

    let navBar =
        Html.nav [
            Html.div [
                prop.className "absolute block ml-12 h-24 w-16 lg:h-36 lg:w-24 bg-purple-500 hover:cursor-pointer hover:bg-purple-400"
                prop.children [
                    Html.a [
                        prop.href "https://www.instagram.com/kunmom_tarot/"
                        prop.className "ml-12 h-24 w-24 rounded-full  hover:cursor-pointer  "
                        prop.children [ Html.img [ prop.src "/image-modified.png"; prop.alt "Kunmom-Tarot" ] ]
                    ]
                ] 
            ]
            Html.div [
                prop.className "flex flex-col"
                prop.children [
                    welCome
                ]
            ]
          
        ]
    
    let footer =
        Html.nav [
            prop.className "flex w-full bg-purple-500 justify-center"
            prop.children [
                Html.div [
                    prop.className "flex "
                    prop.children [
                        Html.div [
                            prop.className "mb-2 ml-12 h-36 w-16 lg:h-full lg:w-36  hover:cursor-pointer"
                            prop.children [
                                Html.a [
                                    prop.href "https://www.instagram.com/kunmom_tarot/"
                                    prop.className "ml-12 h-24 w-24 rounded-full  hover:cursor-pointer  "
                                    prop.children [ Html.img [ prop.src "/image-modified.png"; prop.alt "Kunmom-Tarot" ] ]
                                ]
                            ] 
                        ]
                        Html.div [
                            prop.className "flex flex-col justify-center"
                            prop.children [
                                endFooter
                            ]
                        ]
                ]
            ]
            ]
            
            
          
        ]
    
    let dayDropdown dispatch =
        Html.select [
            prop.className "shadow border hover:bg-purple-200 rounded w-full py-2 px-3"
            prop.onChange ( fun (fn: Event) -> 
                let day = int ((fn.target:?> HTMLSelectElement).value)
                dispatch (SetDate day))
            prop.children [
                for i in 1 .. 31 do
                    Html.option [
                        prop.value (string i)
                        prop.text (string i)
                    ]
            ]
    ]
    let months = ["มกราคม";"กุมาพันธ์";"มีนาคม";"เมษายน";"พฤษภาคม";"มิถุนายน";"กรกฎาคม";"สิงหาคม";"กันยายน";"ตุลาคม";"พฤศจิกายน";"ธันวาคม"]

    let monthDropdown dispatch =
        Html.select [
            prop.className "shadow border hover:bg-purple-200 rounded w-full py-2 px-3"
            prop.onChange ( fun (fn: Event) -> 
                let month = (fn.target:?> HTMLSelectElement).value
                dispatch (SetMonth month))
            prop.children [
                for i in months do
                    Html.option [
                        prop.value (string i)
                        prop.text (string i)
                    ]
            ]
        ]

    let yearDropdown dispatch =
        let currentYear = System.DateTime.Now.Year
        Html.select [
            prop.className "shadow border hover:bg-purple-200 rounded w-full py-2 px-3"
            prop.children [
                for i in currentYear .. -1 .. 1900 do
                    Html.option [
                        prop.value (string i)
                        prop.text (string i)
                    ]
                
            ]
        ]


    let showZodiac model = 
        Html.div [
                prop.className "flex flex-col bg-white/80 rounded-md shadow-xl items-center mt-5 "
                prop.children [
                    match model.Zodiac with
                    | NotStarted -> Html.text "ยังไม่เริ่มการประมวล"
                    | Loading -> Html.text "กำลังประมวลผล..." 
                    | Loaded zodiac -> 
                        Html.div [
                            prop.text $"ราศีของคุณคือ ราศี{zodiac.[0].Zodiac} ธาตุ{zodiac.[0].Elemental} "
                        ]
                        Html.div [
                            prop.text $"{zodiac.[0].Description} "
                        ]
                    
                ]
            ]
    
    let doZodiac model dispatch =
        
        Html.div [
            prop.className "flex flex-col bg-pink-200 rounded-md shadow-md p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
            prop.children [               
                Html.a [
                    prop.className "text-2xl font-bold text-purple-950 text-center m-3"
                    prop.text "ตรวจสอบราศีประจำตัวได้ที่นี่"
                    
                ]
                Html.a [
                    prop.className "text-md text-purple-950"
                    prop.text "เลือกวันเดือนปีเกิดของคุณได้เลย ⇣"
                ]
                Html.div [
                    prop.className "flex flex-row gap-4 mt-2"
                    prop.children [
                        dayDropdown dispatch
                        monthDropdown dispatch
                        yearDropdown dispatch
                        Html.button [
                        prop.className "p-2 bg-yellow-300 rounded hover:bg-yellow-400"
                        prop.text "ตกลง"
                        
                        prop.onClick (fun _ -> 
                            match model.SelectedDay, model.SelectedMonth with
                            | selectedDay, selectedMonth -> dispatch (LoadDestiny (Start (selectedDay, selectedMonth)))
                        )
                        ]
                    
                    ]
                ]
                if model.showZodiac then showZodiac model
            ]
        ]

    

    let modalCard model dispatch =
        Html.div [
            prop.className "fixed inset-0 flex justify-center items-center bg-black/50"
            prop.children [
                Html.div [
                    prop.className "flex mt-10 bg-white rounded-md shadow-2xl p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
                    prop.children [
                        Html.div [
                            prop.className "flex flex-col w-full h-full"
                            prop.children [
                                match model.Card with
                                    | NotStarted -> Html.text "ยังไม่เริ่มการประมวล"
                                    | Loading -> Html.text "กำลังประมวลผล..." 
                                    | Loaded card ->
                                Html.div [
                                    prop.className "flex flex-row"
                                    prop.children [
                                        Html.a [
                                            prop.className "flex items-center justify-center rounded-2xl bg-white/80 h-96 w-72 mb-2 "
                                            prop.children [ 
                                                Html.img [ 
                                                    prop.className "flex h-80 w-60"
                                                    prop.src $"/card/{card.[0].CardId}.png"
                                                    prop.alt "Kunmom-Tarot-Card" 
                                                ] 
                                            ]
                                        ] 
                                        Html.div [
                                            prop.className "flex flex-col  w-1/2 justify-around"
                                            prop.children [
                                                    Html.div [
                                                        prop.className "flex justify-center"
                                                        prop.children [
                                                            Html.div [ 
                                                            prop.className "text-xl font-bold"
                                                            prop.text $"{card.[0].CardName}" 
                                                        ]
                                                        ]
                                                    ]
                                                    Html.div [ 
                                                        prop.text $"ไพ่ใบนี้หมายถึง : {card.[0].CardDescription}"
                                                    ]
                                                    Html.div [ 
                                                        prop.text $"ความรัก : {card.[0].CardLove}"
                                                    ]
                                                    Html.div [ 
                                                        prop.text $"การงาน : {card.[0].CardWork}"
                                                    ]
                                                    Html.div [ 
                                                        prop.text $"การเงิน : {card.[0].CardMoney}"
                                                    ]
                                                    Html.div [ 
                                                        prop.text $"บทสรุป : {card.[0].CardTotal}"
                                                    ]
                                            ]
                                        ]
                                    ]
                                ]
                                Html.div [
                                    prop.className "mt-5"
                                    prop.children [
                                        Html.div [
                                            prop.className "text-sm text-red-600"
                                            prop.text "*แนะนำให้กดบันทึก หากคุณกดปิดไป คุณจะไม่สามารถเปิดไพ่ใบนี้กลับขึ้นมาอีกได้ "
                                        ]
                                        Html.div [
                                            prop.className "text-sm text-red-600"
                                            prop.text "*หากมีไพ่ที่บันทึกอยู่ในตาราง เมื่อบันทึกใหม่ ไพ่ใบเก่าจะหายไป ヾ(•ω•`)o "
                                        ]
                                    ]
                                    
                                    
                                ]
                                Html.div [
                                    prop.className "flex justify-end"
                                    prop.children [
                                        Html.button [
                                            prop.className "flex bg-green-400 hover:bg-green-500 justify-center items-center rounded w-20 h-10"
                                            prop.text "บันทึก"
                                            prop.onClick (fun _ ->
                                                let currentDate = System.DateTime.Now.ToString("dd/MMMM/yyyy")
                                                let chooseCard = card.[0].CardName
                                                let chooseId = card.[0].CardId
                                                dispatch (SetCardId chooseId)
                                                dispatch (AddCard (Start (chooseId,currentDate, chooseCard)))
                                            ) 
                                        ] 
                                        Html.button [
                                            prop.className "flex bg-red-400 hover:bg-red-500 justify-center items-center rounded w-20 h-10 ml-2"
                                            prop.text "ปิด" 
                                            prop.onClick (fun _ -> dispatch CloseModal)
                                        ] 
                                    ]
                                ]
                                
                            ]
                        ]
                    ]
                ]
            ]
        ]

    
    let doCard model dispatch = 
        let currentTime = System.DateTime.Now.ToString("dddd ที่ d MMMM yyyy")
        Html.div [
            prop.className "flex flex-col m-10 bg-pink-200 rounded-md shadow-2xl p-4 w-5/6 lg:w-3/4 lg:max-w-2xl"
            prop.children [               
                Html.a [
                    prop.className "text-xl font-bold text-purple-950 text-center m-3"
                    prop.text $"เปิดไพ่ทำนายดวงประจำวัน {currentTime}"
                    
                ]
                Html.a [
                    prop.className "text-md text-purple-950"
                    prop.text "การเปิดไพ่ทำนายดวงประจำวัน เป็นเพียงการทำนายสิ่งที่จะเกิดขึ้นภายในวันนี้เท่านั้น ซึ่งอาจจะเกิดขึ้นหรือไม่แล้วแต่กำลังดวงค่ะ"
                ]
                Html.a [
                    prop.className "flex justify-center  text-md text-purple-950 m-5 "
                    prop.text "ตั้งสมาธิให้แน่วแน่แล้วกดเปิดไพ่เพื่อดูคำทำนาย (´◡`)"
                ]
                Html.div [
                    prop.className "flex flex-col items-center"
                    prop.children [
                        Html.a [
                        prop.className "flex items-center justify-center rounded-2xl bg-white/80 h-96 w-72 mb-2 "
                        prop.children [ 
                            Html.img [ 
                                prop.className "flex h-80 w-60"
                                prop.src "/back.png"
                                prop.alt "Kunmom-Tarot" 
                            ] 
                        ]] 
                        Html.button [
                            prop.className "p-2 bg-yellow-300 rounded hover:bg-yellow-400"
                            prop.text "เปิดคำทำนาย"
                            prop.onClick (fun _ -> dispatch LoadCardAndShowModal)
                        ]  
                    ]
                    
                ]
                
            ]  
        ]
    
    let FavoriteCard model dispatch = 
        Html.div [
            prop.className "flex flex-col items-center mt-5 mb-10 bg-pink-200 rounded-md shadow-2xl p-4 w-5/6 lg:w-3/4 lg:max-w-2xl "
            prop.children [
                Html.div [
                    prop.className "text-xl font-bold text-purple-950 mb-5 "
                    prop.text "ไพ่ใบที่คุณชื่นชอบประจำวันนี้ ★"
                ]
                
                Html.table [
                    prop.className "fix table-auto w-full border-spacing-3 "
                    prop.children [
                        Html.thead [
                                Html.tr [ 
                                    Html.th [ prop.className "border px-4 py-2";prop.text "วันที่" ]
                                    Html.th [ prop.className "border px-4 py-2";prop.text "ชื่อไพ่" ]
                                    Html.th [ prop.className "border px-4 py-2";prop.text "เรียกดู" ]
                                    Html.th [ prop.className "border px-4 py-2";prop.text "ลบ" ] 
                                ]      
                        ]
                        match model.FavCard with
                        | NotStarted -> 
                            Html.div [
                                prop.className "flex justify-center m-5 text-yellow-800"
                                prop.text $"ยังไม่ประมวลผล" 
                            ]
                        | Loading -> Html.text "กำลังประมวลผล..." 
                        | Loaded card ->
                            if card = [] then 
                                Html.div [
                                    prop.className "flex mt-6 justify-center"
                                    prop.text "ยังไม่มีข้อมูลไพ่ที่คุณชื่นชอบ (⊙_⊙)？"
                                ]
                                
                            else
                            Html.tbody [
                                Html.tr [
                                    Html.td [ prop.className "border px-4 py-2"; prop.text $"{card.[0].Date}" ]
                                    Html.td [ prop.className "border px-4 py-2"; prop.text  $"{card.[0].CardName}"]
                                    Html.td [ 
                                        prop.className "border px-4 py-2 w-1 h-1"; 
                                        prop.children [
                                            Html.button [
                                                prop.className "border border-cyan-500 shadow-lg w-12 h-8 rounded-md bg-cyan-500 hover:bg-cyan-400 hover:border-cyan-400 "
                                                prop.text "กดดู"
                                                // prop.onKeyPress (fun _ -> dispatch (SetCardName card.[0].CardName))
                                                // prop.onClick (fun _ ->
                                                //     match model.SelectedCardName with
                                                //     | selectCardName -> dispatch (LoadViewCard (Start(card.[0].CardName))))
                                                prop.onClick (fun _ -> dispatch (LoadViewCardAndShowModal))
                                            ]
                                        ] 
                                    ]
                                    Html.td [ 
                                        prop.className "border px-4 py-2 w-14 h-14 hover:cursor-pointer "; 
                                        prop.children [ 
                                            Html.button [
                                                prop.className "border border-red-500 shadow-lg w-12 h-8 rounded-md bg-red-500 hover:bg-red-400 hover:border-red-400 "
                                                prop.text "กดลบ"
                                                prop.onClick (fun _ -> dispatch (DeleteCard(Start())))
                                            ]
                                        ] ]
                                    
                                ]
                            ]
                        Html.text $"{model.SelectedId}"
                    ]
                ]
            ]
        ]

    

let view model dispatch =
    Html.section [
        prop.children [
            ViewComponents.navBar
            Html.div [
                prop.className "flex flex-col items-center"
                prop.children [
                    ViewComponents.doZodiac model dispatch
                    ViewComponents.doCard model dispatch
                    // if model.showModal then ViewComponents.modalCard model dispatch
                    ViewComponents.FavoriteCard model dispatch
                    if model.showModal then ViewComponents.modalCard model dispatch
                    ViewComponents.footer 
                ]
            
            ]
            
            
        ]
    ]
