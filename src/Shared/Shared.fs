namespace Shared

open System

type Fortune = 
    {
        Zodiac: string
        Elemental: string
        Description: string
    }
type Zodiac = 
    {
        day: int
        month: string
    }

type Card =
    {
        CardId: int
        CardName : string
        CardDescription : string
        CardLove : string
        CardWork: string
        CardMoney: string
        CardTotal:string

    }

type Room = 
    {
        Id : int
        Date : string
        CardName : string
    }

type ZodiacError =
    | NoReason

type ZodiacValidateError =
    | ZodiacError of ZodiacError



// type Todo = { 
//     Id: int; 
//     Description: string 
// }


// module Todo =
//     let isValid (description: string) =
//         String.IsNullOrWhiteSpace description |> not

//     let create (id: int) (description: string) = {
//         Id = id
//         Description = description
//     }

// type ITodosApi = {
//     getTodos: unit -> Async<Todo list>
//     addTodo: Todo -> Async<Todo>
// }

type ICardApi = {
    getZodiac: int*string -> Async<Fortune list>
    getCard: unit -> Async<Card list>
    getFavoriteCard : unit -> Async<Room list>
    getViewCard : int -> Async<Card list>
    // addCard: string*string -> Async<string>
    addOrupdateCard : int*string*string -> Async<string>
    deleteCard: unit -> Async<string>
}